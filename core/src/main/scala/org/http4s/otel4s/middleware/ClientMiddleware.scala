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
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.ResponsePrelude
import org.http4s.client.Client
import org.http4s.client.RequestKey
import org.http4s.client.middleware.Retry
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.vault.Key
import org.typelevel.vault.Vault

import scala.collection.immutable

/** Middleware for wrapping an http4s `Client` to add tracing. */
object ClientMiddleware {

  /** @return a client middleware builder with default configuration */
  def default[F[_]: Tracer: Concurrent]: ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.clientSpanName,
      Defaults.additionalRequestAttributes,
      Defaults.additionalResponseAttributes,
      Defaults.urlRedactor,
    )

  /** The default configuration values for a client middleware builder. */
  object Defaults {
    def allowedRequestHeaders: Set[CIString] =
      TypedAttributes.Headers.defaultAllowedHeaders
    def allowedResponseHeaders: Set[CIString] =
      TypedAttributes.Headers.defaultAllowedHeaders
    val clientSpanName: RequestPrelude => String =
      req => s"Http Client - ${req.method}"
    val additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]] =
      _ => Nil
    val additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]] =
      _ => Nil
    def urlRedactor: UriRedactor = UriRedactor.OnlyRedactUserInfo
  }

  /** A builder for client middlewares. */
  final class ClientMiddlewareBuilder[F[_]: Tracer: Concurrent] private[ClientMiddleware] (
      private val allowedRequestHeaders: Set[CIString],
      private val allowedResponseHeaders: Set[CIString],
      private val clientSpanName: RequestPrelude => String,
      private val additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]],
      private val additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]],
      private val urlRedactor: UriRedactor,
  ) {
    private def copy(
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        clientSpanName: RequestPrelude => String = this.clientSpanName,
        additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]] =
          this.additionalRequestAttributes,
        additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]] =
          this.additionalResponseAttributes,
        urlRedactor: UriRedactor = this.urlRedactor,
    ): ClientMiddlewareBuilder[F] =
      new ClientMiddlewareBuilder[F](
        allowedRequestHeaders,
        allowedResponseHeaders,
        clientSpanName,
        additionalRequestAttributes,
        additionalResponseAttributes,
        urlRedactor,
      )

    /** Sets which request headers are allowed to made into `Attribute`s. */
    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): ClientMiddlewareBuilder[F] =
      copy(allowedRequestHeaders = allowedHeaders)

    /** Sets which response headers are allowed to made into `Attribute`s. */
    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): ClientMiddlewareBuilder[F] =
      copy(allowedResponseHeaders = allowedHeaders)

    /** Sets how to derive the name of a client span from a request. */
    def withClientSpanName(clientSpanName: RequestPrelude => String): ClientMiddlewareBuilder[F] =
      copy(clientSpanName = clientSpanName)

    /** Sets how to derive additional `Attribute`s from a request to add to the
      *  client span.
      */
    def withAdditionalRequestAttributes(
        additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]]
    ): ClientMiddlewareBuilder[F] =
      copy(additionalRequestAttributes = additionalRequestAttributes)

    /** Sets how to derive additional `Attribute`s from a response to add to the
      *  client span.
      */
    def withAdditionalResponseAttributes(
        additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]]
    ): ClientMiddlewareBuilder[F] =
      copy(additionalResponseAttributes = additionalResponseAttributes)

    /** Sets how to redact URLs before turning them into `Attribute`s. */
    def withUrlRedactor(urlRedactor: UriRedactor): ClientMiddlewareBuilder[F] =
      copy(urlRedactor = urlRedactor)

    /** @return the configured middleware */
    def build: Client[F] => Client[F] = (client: Client[F]) =>
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]
        val reqPrelude = req.requestPrelude
        val base =
          request(req, allowedRequestHeaders, urlRedactor) ++
            additionalRequestAttributes(reqPrelude)
        MonadCancelThrow[Resource[F, *]].uncancelable { poll =>
          for {
            res <- Tracer[F]
              .spanBuilder(
                req.attributes.lookup(OverrideSpanNameKey).getOrElse(clientSpanName(reqPrelude))
              )
              .withSpanKind(SpanKind.Client)
              .addAttributes(base)
              .build
              .resource
            span = res.span
            trace = res.trace
            traceHeaders <- Resource.eval(Tracer[F].propagate(Headers.empty)).mapK(trace)
            newReq = req.withHeaders(traceHeaders ++ req.headers)
            resp <- poll(client.run(newReq)).guaranteeCase { outcome =>
              (outcome match {
                case Outcome.Succeeded(fa) =>
                  fa.flatMap { resp =>
                    Resource
                      .eval {
                        span.addAttributes(
                          response(resp, allowedResponseHeaders) ++
                            additionalResponseAttributes(resp.responsePrelude)
                        )
                      }
                      .mapK(trace)
                  }
                case Outcome.Errored(e) =>
                  Resource.eval(span.recordException(e)).mapK(trace)
                case Outcome.Canceled() =>
                  // Canceled isn't always an error, but it generally is for http
                  // TODO: decide if this should add "error", we do for the server side.
                  Resource.eval(span.addAttribute(CustomAttributes.Canceled(true))).mapK(trace)
              }) >> Resource.eval(span.addAttribute(CustomAttributes.exitCase(outcome))).mapK(trace)
            }
            // Automatically handle client processing errors. Since this is after the response,
            // the error case will only get hit if the use block of the resulting resource happens,
            // which is the request processing stage.
            _ <- Resource
              .makeCase(Applicative[F].unit) {
                case (_, Resource.ExitCase.Errored(e)) => span.recordException(e)
                case (_, _) => Applicative[F].unit
              }
              .mapK(trace)
          } yield resp
        }
      }
  }

  /** A key used to attach additional `Attribute`s to a request or response. */
  val ExtraAttributesKey: Key[Attributes] =
    Key.newKey[SyncIO, Attributes].unsafeRunSync()

  /** A key used to override the span name for a specific request. If set, this attribute takes precedence over
    *  anything configured through [[ClientMiddlewareBuilder.withClientSpanName]].
    */
  val OverrideSpanNameKey: Key[String] =
    Key.newKey[SyncIO, String].unsafeRunSync()

  /** @return the default `Attribute`s for a request */
  private def request[F[_]](
      request: Request[F],
      allowedHeaders: Set[CIString],
      urlRedactor: UriRedactor,
  ): Attributes = {
    val builder = Attributes.newBuilder
    builder += TypedAttributes.httpRequestMethod(request.method)
    builder ++= TypedAttributes.url(request.uri, urlRedactor)
    val host = request.headers.get[Host].getOrElse {
      val key = RequestKey.fromRequest(request)
      Host(key.authority.host.value, key.authority.port)
    }
    builder += TypedAttributes.serverAddress(host)
    request.headers
      .get[`User-Agent`]
      .foreach(ua => builder += TypedAttributes.userAgentOriginal(ua))

    request.remote.foreach { socketAddress =>
      builder +=
        TypedAttributes.networkPeerAddress(socketAddress.host)

      builder +=
        TypedAttributes.Client.serverPort(socketAddress.port)

    }
    retryCount(request.attributes).foreach { count =>
      builder += TypedAttributes.httpRequestResendCount(count.toLong)
    }
    builder ++=
      TypedAttributes.Headers.request(request.headers, allowedHeaders)

    request.attributes.lookup(ExtraAttributesKey).foreach(builder ++= _)

    builder.result()
  }

  /** @return the default `Attribute`s for a response */
  private def response[F[_]](response: Response[F], allowedHeaders: Set[CIString]): Attributes = {
    val builder = Attributes.newBuilder

    builder += TypedAttributes.httpResponseStatusCode(response.status)
    retryCount(response.attributes).foreach { count =>
      builder += TypedAttributes.httpRequestResendCount(count.toLong)
    }

    builder ++= TypedAttributes.Headers.response(response.headers, allowedHeaders)
    builder ++= response.attributes.lookup(ExtraAttributesKey).toList.flatten

    builder.result()
  }

  private def retryCount(vault: Vault): Option[Int] =
    // AttemptCountKey is 1,2,3,4 for the initial request,
    // since we want to do retries. We substract by 1 to get 0,1,2,3.
    vault.lookup(Retry.AttemptCountKey).map(i => i - 1)

}
