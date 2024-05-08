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

import scala.collection.immutable

/** Middleware for wrapping an http4s `Server` to add tracing. */
object ServerMiddleware {

  /** @return a server middleware builder with default configuration */
  def default[F[_]: Tracer: MonadCancelThrow]: ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.routeClassifier,
      Defaults.serverSpanName,
      Defaults.additionalRequestAttributes,
      Defaults.additionalResponseAttributes,
      Defaults.urlRedactor,
      Defaults.shouldTrace,
    )

  /** The default configuration values for a server middleware builder. */
  object Defaults {
    val allowedRequestHeaders: Set[CIString] = TypedAttributes.Headers.defaultAllowedHeaders
    val allowedResponseHeaders: Set[CIString] = TypedAttributes.Headers.defaultAllowedHeaders
    def routeClassifier[F[_]]: Request[F] => Option[String] =
      (_: Request[F]) => None
    def serverSpanName[F[_]]: Request[F] => String =
      (req: Request[F]) => s"Http Server - ${req.method}"
    def additionalRequestAttributes[F[_]]: Request[F] => immutable.Iterable[Attribute[_]] =
      (_: Request[F]) => Nil
    def additionalResponseAttributes[F[_]]: Response[F] => immutable.Iterable[Attribute[_]] =
      (_: Response[F]) => Nil
    val urlRedactor: UriRedactor = UriRedactor.OnlyRedactUserInfo
    val shouldTrace: RequestPrelude => ShouldTrace =
      (_: RequestPrelude) => ShouldTrace.Trace
  }

  /** A builder for server middlewares. */
  final class ServerMiddlewareBuilder[F[_]: Tracer: MonadCancelThrow] private[ServerMiddleware] (
      allowedRequestHeaders: Set[CIString],
      allowedResponseHeaders: Set[CIString],
      routeClassifier: Request[F] => Option[String],
      serverSpanName: Request[F] => String,
      additionalRequestAttributes: Request[F] => immutable.Iterable[Attribute[_]],
      additionalResponseAttributes: Response[F] => immutable.Iterable[Attribute[_]],
      urlRedactor: UriRedactor,
      shouldTrace: RequestPrelude => ShouldTrace,
  ) {
    private def copy(
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        routeClassifier: Request[F] => Option[String] = this.routeClassifier,
        serverSpanName: Request[F] => String = this.serverSpanName,
        additionalRequestAttributes: Request[F] => immutable.Iterable[Attribute[_]] =
          this.additionalRequestAttributes,
        additionalResponseAttributes: Response[F] => immutable.Iterable[Attribute[_]] =
          this.additionalResponseAttributes,
        urlRedactor: UriRedactor = this.urlRedactor,
        shouldTrace: RequestPrelude => ShouldTrace = this.shouldTrace,
    ): ServerMiddlewareBuilder[F] =
      new ServerMiddlewareBuilder[F](
        allowedRequestHeaders,
        allowedResponseHeaders,
        routeClassifier,
        serverSpanName,
        additionalRequestAttributes,
        additionalResponseAttributes,
        urlRedactor,
        shouldTrace,
      )

    /** Sets which request headers are allowed to made into `Attribute`s. */
    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(allowedRequestHeaders = allowedHeaders)

    /** Sets which response headers are allowed to made into `Attribute`s. */
    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(allowedResponseHeaders = allowedHeaders)

    /** Sets how to determine the route within the application from a request.
      * A value of `None` returned by the given function indicates that the
      * route could not be determined.
      */
    def withRouteClassifier(
        routeClassifier: Request[F] => Option[String]
    ): ServerMiddlewareBuilder[F] =
      copy(routeClassifier = routeClassifier)

    /** Sets how to derive the name of a server span from a request. */
    def withServerSpanName(serverSpanName: Request[F] => String): ServerMiddlewareBuilder[F] =
      copy(serverSpanName = serverSpanName)

    /** Sets how to derive additional `Attribute`s from a request to add to the
      *  server span.
      */
    def withAdditionalRequestAttributes(
        additionalRequestAttributes: Request[F] => immutable.Iterable[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalRequestAttributes = additionalRequestAttributes)

    /** Sets how to derive additional `Attribute`s from a response to add to the
      *  server span.
      */
    def withAdditionalResponseAttributes(
        additionalResponseAttributes: Response[F] => immutable.Iterable[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalResponseAttributes = additionalResponseAttributes)

    /** Sets how to redact URLs before turning them into `Attribute`s. */
    def withUrlRedactor(urlRedactor: UriRedactor): ServerMiddlewareBuilder[F] =
      copy(urlRedactor = urlRedactor)

    /** Sets how to determine when to trace a request and its response. */
    def withShouldTrace(shouldTrace: RequestPrelude => ShouldTrace): ServerMiddlewareBuilder[F] =
      copy(shouldTrace = shouldTrace)

    /** Returns a middleware in a way that abstracts over
      * [[org.http4s.HttpApp `HttpApp`]] and
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
        if (
          !shouldTrace(req.requestPrelude).shouldTrace ||
          !Tracer[F].meta.isEnabled
        ) {
          f(req)
        } else {
          val init =
            request(
              req,
              allowedRequestHeaders,
              routeClassifier,
              urlRedactor,
            ) ++ additionalRequestAttributes(req)
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
                              response(
                                resp,
                                allowedResponseHeaders,
                              ) ++ additionalResponseAttributes(resp)
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

    /** @return a configured middleware for `HttpApp` */
    def buildHttpApp(f: HttpApp[F]): HttpApp[F] =
      buildGenericTracedHttp(f)

    /** @return a configured middleware for `HttpRoutes` */
    def buildHttpRoutes(f: HttpRoutes[F]): HttpRoutes[F] =
      buildGenericTracedHttp(f)
  }

  /** @return the default `Attribute`s for a request */
  private def request[F[_]](
      request: Request[F],
      allowedHeaders: Set[CIString],
      routeClassifier: Request[F] => Option[String],
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

  /** @return the default `Attribute`s for a response */
  private def response[F[_]](response: Response[F], allowedHeaders: Set[CIString]): Attributes = {
    val builder = Attributes.newBuilder

    builder += TypedAttributes.httpResponseStatusCode(response.status)
    builder ++= TypedAttributes.Headers.response(response.headers, allowedHeaders)

    builder.result()
  }
}
