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
package trace.server

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
import org.http4s.ResponsePrelude
import org.http4s.Uri
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.http4s.otel4s.middleware.trace.internal.MiddlewareBuilder
import org.http4s.otel4s.middleware.trace.internal.MiddlewareDefaults
import org.http4s.otel4s.middleware.trace.internal.TraceAttributes
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
  def builder[F[_]: Tracer: MonadCancelThrow]: Builder[F] =
    new Builder[F](
      MiddlewareDefaults.allowedRequestHeaders,
      MiddlewareDefaults.allowedResponseHeaders,
      Defaults.routeClassifier,
      Defaults.spanName,
      MiddlewareDefaults.additionalRequestAttributes,
      MiddlewareDefaults.additionalResponseAttributes,
      MiddlewareDefaults.urlRedactor,
      Defaults.shouldTrace,
    )

  /** The default configuration values for a server middleware builder. */
  private object Defaults {
    val routeClassifier: RequestPrelude => Option[String] = _ => None
    val spanName: RequestPrelude => String = req => s"Http Server - ${req.method}"
    val shouldTrace: RequestPrelude => ShouldTrace = _ => ShouldTrace.Trace
  }

  /** A builder for server middlewares. */
  final class Builder[F[_]: Tracer: MonadCancelThrow] private[ServerMiddleware] (
      protected val allowedRequestHeaders: Set[CIString],
      protected val allowedResponseHeaders: Set[CIString],
      routeClassifier: RequestPrelude => Option[String],
      protected val spanName: RequestPrelude => String,
      protected val additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]],
      protected val additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[
        _
      ]],
      protected val urlRedactor: UriRedactor,
      shouldTrace: RequestPrelude => ShouldTrace,
  ) extends MiddlewareBuilder[Builder, F] {
    override protected def copyShared(
        allowedRequestHeaders: Set[CIString],
        allowedResponseHeaders: Set[CIString],
        spanName: RequestPrelude => String,
        additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]],
        additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]],
        urlRedactor: UriRedactor = this.urlRedactor,
    ): Builder[F] =
      new Builder[F](
        allowedRequestHeaders,
        allowedResponseHeaders,
        this.routeClassifier,
        spanName,
        additionalRequestAttributes,
        additionalResponseAttributes,
        urlRedactor,
        this.shouldTrace,
      )

    private def copy(
        routeClassifier: RequestPrelude => Option[String] = this.routeClassifier,
        shouldTrace: RequestPrelude => ShouldTrace = this.shouldTrace,
    ): Builder[F] =
      new Builder[F](
        this.allowedRequestHeaders,
        this.allowedResponseHeaders,
        routeClassifier,
        this.spanName,
        this.additionalRequestAttributes,
        this.additionalResponseAttributes,
        this.urlRedactor,
        shouldTrace,
      )

    /** Sets how to determine the route within the application from a request.
      * A value of `None` returned by the given function indicates that the
      * route could not be determined.
      */
    def withRouteClassifier(
        routeClassifier: RequestPrelude => Option[String]
    ): Builder[F] =
      copy(routeClassifier = routeClassifier)

    /** Sets how to determine when to trace a request and its response. */
    def withShouldTrace(shouldTrace: RequestPrelude => ShouldTrace): Builder[F] =
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
        val reqPrelude = req.requestPrelude
        if (
          !shouldTrace(reqPrelude).shouldTrace ||
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
            ) ++ additionalRequestAttributes(reqPrelude)
          MonadCancelThrow[G].uncancelable { poll =>
            val tracerG = Tracer[F].mapK[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG
                .spanBuilder(spanName(reqPrelude))
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
                              ) ++ additionalResponseAttributes(resp.responsePrelude)
                            span.addAttributes(out)
                          }
                        case Outcome.Errored(e) =>
                          span.recordException(e)
                        case Outcome.Canceled() =>
                          span.addAttributes(
                            TraceAttributes.Canceled(true),
                            TraceAttributes.Error(
                              true
                            ), // A canceled http is an error for the server. The connection got cut for some reason.
                          )
                      }) >> span.addAttribute(TraceAttributes.exitCase(outcome))
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
      routeClassifier: RequestPrelude => Option[String],
      urlRedactor: UriRedactor,
  ): Attributes = {
    val builder = Attributes.newBuilder
    builder += TypedAttributes.httpRequestMethod(request.method)
    builder ++= TypedAttributes.url(request.uri, urlRedactor)
    val host = request.headers.get[Host].getOrElse {
      val authority = request.uri.authority.getOrElse(Uri.Authority())
      Host(authority.host.value, authority.port)
    }
    builder += TypedAttributes.serverAddress(host)
    request.headers
      .get[`User-Agent`]
      .foreach(ua => builder += TypedAttributes.userAgentOriginal(ua))

    routeClassifier(request.requestPrelude).foreach(route =>
      builder += TypedServerAttributes.httpRoute(route)
    )

    request.remote.foreach { socketAddress =>
      builder +=
        TypedAttributes.networkPeerAddress(socketAddress.host)

      builder +=
        TypedServerAttributes.clientPort(socketAddress.port)
    }

    TypedServerAttributes.clientAddress(request).foreach(builder += _)
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
