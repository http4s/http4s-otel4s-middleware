/*
 * Copyright 2023 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.otel4s.middleware

import cats.Applicative
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.SyncIO
import cats.effect.kernel.Outcome
import cats.syntax.flatMap._
import org.http4s.Headers
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.client.RequestKey
import org.http4s.client.middleware.Retry
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.vault.Key
import org.typelevel.vault.Vault

object ClientMiddleware {

  def default[F[_]: Tracer: Concurrent]: ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.clientSpanName,
      Defaults.additionalRequestTags,
      Defaults.additionalResponseTags,
      Defaults.includeUrl,
    )

  object Defaults {
    val allowedRequestHeaders: Set[CIString] = HttpAttributes.Headers.defaultAllowedHeaders
    val allowedResponseHeaders: Set[CIString] = HttpAttributes.Headers.defaultAllowedHeaders
    def clientSpanName[F[_]]: Request[F] => String = { (req: Request[F]) =>
      s"Http Client - ${req.method}"
    }
    def additionalRequestTags[F[_]]: Request[F] => Seq[Attribute[_]] = { (_: Request[F]) => Seq() }
    def additionalResponseTags[F[_]]: Response[F] => Seq[Attribute[_]] = { (_: Response[F]) =>
      Seq()
    }
    def includeUrl[F[_]]: Request[F] => Boolean = { (_: Request[F]) => true }
  }

  final class ClientMiddlewareBuilder[F[_]: Tracer: Concurrent] private[ClientMiddleware] (
      private val allowedRequestHeaders: Set[CIString],
      private val allowedResponseHeaders: Set[CIString],
      private val clientSpanName: Request[F] => String,
      private val additionalRequestTags: Request[F] => Seq[Attribute[_]],
      private val additionalResponseTags: Response[F] => Seq[Attribute[_]],
      private val includeUrl: Request[F] => Boolean,
  ) {
    private def copy(
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        clientSpanName: Request[F] => String = this.clientSpanName,
        additionalRequestTags: Request[F] => Seq[Attribute[_]] = this.additionalRequestTags,
        additionalResponseTags: Response[F] => Seq[Attribute[_]] = this.additionalResponseTags,
        includeUrl: Request[F] => Boolean = this.includeUrl,
    ): ClientMiddlewareBuilder[F] =
      new ClientMiddlewareBuilder[F](
        allowedRequestHeaders,
        allowedResponseHeaders,
        clientSpanName,
        additionalRequestTags,
        additionalResponseTags,
        includeUrl,
      )

    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): ClientMiddlewareBuilder[F] =
      copy(allowedRequestHeaders = allowedHeaders)

    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): ClientMiddlewareBuilder[F] =
      copy(allowedResponseHeaders = allowedHeaders)

    def withClientSpanName(clientSpanName: Request[F] => String): ClientMiddlewareBuilder[F] =
      copy(clientSpanName = clientSpanName)

    def withAdditionalRequestTags(
        additionalRequestTags: Request[F] => Seq[Attribute[_]]
    ): ClientMiddlewareBuilder[F] =
      copy(additionalRequestTags = additionalRequestTags)

    def withAdditionalResponseTags(
        additionalResponseTags: Response[F] => Seq[Attribute[_]]
    ): ClientMiddlewareBuilder[F] =
      copy(additionalResponseTags = additionalResponseTags)

    def withIncludeUrl(includeUrl: Request[F] => Boolean): ClientMiddlewareBuilder[F] =
      copy(includeUrl = includeUrl)

    def build: Client[F] => Client[F] = { (client: Client[F]) =>
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]

        val base = request(req, allowedRequestHeaders, includeUrl) ++ additionalRequestTags(req)
        MonadCancelThrow[Resource[F, *]].uncancelable { poll =>
          for {
            res <- Tracer[F]
              .spanBuilder(clientSpanName(req))
              .withSpanKind(SpanKind.Client)
              .addAttributes(base: _*)
              .build
              .resource
            span = res.span
            traceHeaders <- Resource.eval(Tracer[F].propagate(Headers.empty))
            newReq = req.withHeaders(traceHeaders ++ req.headers)
            resp <- poll(client.run(newReq)).guaranteeCase {
              case Outcome.Succeeded(fa) =>
                Resource.eval(span.addAttribute(Attribute("exit.case", "succeeded"))) >>
                  fa.flatMap(resp =>
                    Resource.eval(
                      span.addAttributes(
                        response(resp, allowedResponseHeaders) ++ additionalResponseTags(resp): _*
                      )
                    )
                  )
              case Outcome.Errored(e) =>
                Resource.eval(
                  span.recordException(e) >>
                    span.addAttribute(Attribute("exit.case", "errored"))
                )
              case Outcome.Canceled() =>
                // Canceled isn't always an error, but it generally is for http
                // TODO decide if this should add error, we do for the server side.
                Resource.eval(
                  span.addAttributes(
                    Attribute("exit.case", "canceled"),
                    Attribute("canceled", true),
                  )
                )

            }
            // Automatically handle client processing errors. Since this is after the response,
            // the error case will only get hit if the use block of the resulting resource happens,
            // which is the request processing stage.
            _ <- Resource.makeCase(Applicative[F].unit) {
              case (_, Resource.ExitCase.Errored(e)) => span.recordException(e)
              case (_, _) => Applicative[F].unit
            }
          } yield resp
        }
      }
    }
  }

  val ExtraAttributesKey: Key[List[Attribute[_]]] =
    Key.newKey[SyncIO, List[Attribute[_]]].unsafeRunSync()

  def request[F[_]](
      request: Request[F],
      allowedHeaders: Set[CIString],
      includeUrl: Request[F] => Boolean,
  ): List[Attribute[_]] = {
    val builder = List.newBuilder[Attribute[_]]
    builder += HttpAttributes.httpRequestMethod(request.method)
    if (includeUrl(request)) {
      builder += HttpAttributes.urlFull(request.uri)
      builder += HttpAttributes.urlPath(request.uri.path)
      builder += HttpAttributes.urlQuery(request.uri.query)
    }
    val host = request.headers.get[Host].getOrElse {
      val key = RequestKey.fromRequest(request)
      Host(key.authority.host.value, key.authority.port)
    }
    builder += HttpAttributes.serverAddress(host)
    request.uri.scheme.foreach(scheme => builder += HttpAttributes.urlScheme(scheme))
    request.headers.get[`User-Agent`].foreach(ua => builder += HttpAttributes.userAgentOriginal(ua))

    request.remote.foreach { socketAddress =>
      builder +=
        HttpAttributes.networkPeerAddress(socketAddress.host)

      builder +=
        HttpAttributes.Client.serverPort(socketAddress.port)

    }
    retryCount(request.attributes).foreach { count =>
      builder += HttpAttributes.httpRequestResendCount(count.toLong)
    }
    builder ++=
      HttpAttributes.Headers.request(request.headers, allowedHeaders)

    builder ++= request.attributes.lookup(ExtraAttributesKey).toList.flatten

    builder.result()
  }

  def response[F[_]](response: Response[F], allowedHeaders: Set[CIString]): List[Attribute[_]] = {
    val builder = List.newBuilder[Attribute[_]]

    builder += HttpAttributes.httpResponseStatusCode(response.status)
    retryCount(response.attributes).foreach { count =>
      builder += HttpAttributes.httpRequestResendCount(count.toLong)
    }

    builder ++= HttpAttributes.Headers.response(response.headers, allowedHeaders)
    builder ++= response.attributes.lookup(ExtraAttributesKey).toList.flatten

    builder.result()
  }

  private def retryCount(vault: Vault): Option[Int] =
    // AttemptCountKey is 1,2,3,4 for the initial request,
    // since we want to do retries. We substract by 1 to get 0,1,2,3.
    vault.lookup(Retry.AttemptCountKey).map(i => i - 1)

}
