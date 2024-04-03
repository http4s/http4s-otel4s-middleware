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

import cats.data.Kleisli
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.flatMap._
import org.http4s.Http
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.client.RequestKey
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer

object ServerMiddleware {

  def default[F[_]: Tracer: MonadCancelThrow]: ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.routeClassifier,
      Defaults.serverSpanName,
      Defaults.additionalRequestTags,
      Defaults.additionalResponseTags,
      Defaults.includeUrl,
      Defaults.doNotTrace,
    )

  object Defaults {
    val allowedRequestHeaders: Set[CIString] = TypedAttributes.Headers.defaultAllowedHeaders
    val allowedResponseHeaders: Set[CIString] = TypedAttributes.Headers.defaultAllowedHeaders
    def routeClassifier[F[_]]: Request[F] => Option[String] = { (_: Request[F]) => None }
    def serverSpanName[F[_]]: Request[F] => String = { (req: Request[F]) =>
      s"Http Server - ${req.method}"
    }
    def additionalRequestTags[F[_]]: Request[F] => Seq[Attribute[_]] = { (_: Request[F]) => Seq() }
    def additionalResponseTags[F[_]]: Response[F] => Seq[Attribute[_]] = { (_: Response[F]) =>
      Seq()
    }
    def includeUrl[F[_]]: Request[F] => Boolean = { (_: Request[F]) => true }
    def doNotTrace: RequestPrelude => Boolean = { (_: RequestPrelude) => false }
  }

  final class ServerMiddlewareBuilder[F[_]: Tracer: MonadCancelThrow] private[ServerMiddleware] (
      allowedRequestHeaders: Set[CIString],
      allowedResponseHeaders: Set[CIString],
      routeClassifier: Request[F] => Option[String],
      serverSpanName: Request[F] => String,
      additionalRequestTags: Request[F] => Seq[Attribute[_]],
      additionalResponseTags: Response[F] => Seq[Attribute[_]],
      includeUrl: Request[F] => Boolean,
      doNotTrace: RequestPrelude => Boolean,
  ) {
    private def copy(
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        routeClassifier: Request[F] => Option[String] = this.routeClassifier,
        serverSpanName: Request[F] => String = this.serverSpanName,
        additionalRequestTags: Request[F] => Seq[Attribute[_]] = this.additionalRequestTags,
        additionalResponseTags: Response[F] => Seq[Attribute[_]] = this.additionalResponseTags,
        includeUrl: Request[F] => Boolean = this.includeUrl,
        doNotTrace: RequestPrelude => Boolean = this.doNotTrace,
    ): ServerMiddlewareBuilder[F] =
      new ServerMiddlewareBuilder[F](
        allowedRequestHeaders,
        allowedResponseHeaders,
        routeClassifier,
        serverSpanName,
        additionalRequestTags,
        additionalResponseTags,
        includeUrl,
        doNotTrace,
      )

    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(allowedRequestHeaders = allowedHeaders)
    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(allowedResponseHeaders = allowedHeaders)
    def withRouteClassifier(
        routeClassifier: Request[F] => Option[String]
    ): ServerMiddlewareBuilder[F] =
      copy(routeClassifier = routeClassifier)
    def withServerSpanName(serverSpanName: Request[F] => String): ServerMiddlewareBuilder[F] =
      copy(serverSpanName = serverSpanName)
    def withAdditionalRequestTags(
        additionalRequestTags: Request[F] => Seq[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalRequestTags = additionalRequestTags)
    def withAdditionalResponseTags(
        additionalResponseTags: Response[F] => Seq[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalResponseTags = additionalResponseTags)
    def withIncludeUrl(includeUrl: Request[F] => Boolean): ServerMiddlewareBuilder[F] =
      copy(includeUrl = includeUrl)
    def withDoNotTrace(doNotTrace: RequestPrelude => Boolean): ServerMiddlewareBuilder[F] =
      copy(doNotTrace = doNotTrace)

    /** This method is used for building a middleware in a way that abstracts
      * over [[org.http4s.HttpApp `HttpApp`]] and
      * [[org.http4s.HttpRoutes `HttpRoutes`]]. In most cases, it is preferable
      * to use the methods that directly build the specific desired type.
      *
      * @see [[buildHttpApp]]
      * @see [[buildHttpRoutes]]
      */
    def buildGenericTracedHttp[G[_]: MonadCancelThrow](
        f: Http[G, F]
    )(implicit kt: KindTransformer[F, G]): Http[G, F] =
      Kleisli { (req: Request[F]) =>
        if (doNotTrace(req.requestPrelude) || !Tracer[F].meta.isEnabled) f(req)
        else {
          val init =
            request(
              req,
              allowedRequestHeaders,
              routeClassifier,
              includeUrl,
            ) ++ additionalRequestTags(req)
          MonadCancelThrow[G].uncancelable { poll =>
            val tracerG = Tracer[F].mapK[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG
                .spanBuilder(serverSpanName(req))
                .withSpanKind(SpanKind.Server)
                .addAttributes(init)
                .build
                .use { span =>
                  poll(f.run(req))
                    .guaranteeCase { outcome =>
                      (outcome match {
                        case Outcome.Succeeded(fa) =>
                          fa.flatMap { resp =>
                            val out =
                              response(resp, allowedResponseHeaders) ++ additionalResponseTags(resp)
                            span.addAttributes(out)
                          }
                        case Outcome.Errored(e) =>
                          span.recordException(e)
                        case Outcome.Canceled() =>
                          span.addAttributes(
                            CustomAttributes.Canceled(true),
                            CustomAttributes.Error(
                              true
                            ), // A canceled http is an error for the server. The connection got cut for some reason.
                          )
                      }) >> span.addAttribute(CustomAttributes.exitCase(outcome))
                    }
                }
            }
          }
        }
      }

    def buildHttpApp(f: HttpApp[F]): HttpApp[F] =
      buildGenericTracedHttp(f)

    def buildHttpRoutes(f: HttpRoutes[F]): HttpRoutes[F] =
      buildGenericTracedHttp(f)
  }

  private[middleware] def request[F[_]](
      req: Request[F],
      headers: Set[CIString],
      routeClassifier: Request[F] => Option[String],
  ): Attributes =
    request(req, headers, routeClassifier, Function.const[Boolean, Request[F]](true))

  def request[F[_]](
      request: Request[F],
      allowedHeaders: Set[CIString],
      routeClassifier: Request[F] => Option[String],
      includeUrl: Request[F] => Boolean,
  ): Attributes = {
    val builder = Attributes.newBuilder
    builder += TypedAttributes.httpRequestMethod(request.method)
    if (includeUrl(request)) {
      builder += TypedAttributes.urlFull(request.uri)
      builder += TypedAttributes.urlPath(request.uri.path)
      builder += TypedAttributes.urlQuery(request.uri.query)
    }
    val host = request.headers.get[Host].getOrElse {
      val key = RequestKey.fromRequest(request)
      Host(key.authority.host.value, key.authority.port)
    }
    builder += TypedAttributes.serverAddress(host)
    request.uri.scheme.foreach(s => builder += TypedAttributes.urlScheme(s))
    request.headers
      .get[`User-Agent`]
      .foreach(ua => builder += TypedAttributes.userAgentOriginal(ua))

    routeClassifier(request).foreach(route => builder += TypedAttributes.Server.httpRoute(route))

    request.remote.foreach { socketAddress =>
      builder +=
        TypedAttributes.networkPeerAddress(socketAddress.host)

      builder +=
        TypedAttributes.Server.clientPort(socketAddress.port)
    }

    TypedAttributes.Server.clientAddress(request).foreach(builder += _)
    builder ++=
      TypedAttributes.Headers.request(request.headers, allowedHeaders)

    builder.result()
  }

  def response[F[_]](response: Response[F], allowedHeaders: Set[CIString]): Attributes = {
    val builder = Attributes.newBuilder

    builder += TypedAttributes.httpResponseStatusCode(response.status)
    builder ++= TypedAttributes.Headers.response(response.headers, allowedHeaders)

    builder.result()
  }
}
