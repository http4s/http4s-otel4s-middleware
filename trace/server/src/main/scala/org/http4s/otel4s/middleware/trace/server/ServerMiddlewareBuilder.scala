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

package org.http4s
package otel4s.middleware
package trace
package server

import cats.data.Kleisli
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import fs2.Stream
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

/** Middleware builder for wrapping an http4s `Server` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  */
class ServerMiddlewareBuilder[F[_]: Tracer: MonadCancelThrow] private (
    redactor: PathAndQueryRedactor, // cannot safely have default value
    spanDataProvider: SpanDataProvider,
    routeClassifier: RouteClassifier,
    headersAllowedAsAttributes: HeadersAllowedAsAttributes,
    perRequestTracingFilter: PerRequestTracingFilter,
) {
  private def copy(
      spanDataProvider: SpanDataProvider = this.spanDataProvider,
      routeClassifier: RouteClassifier = this.routeClassifier,
      headersAllowedAsAttributes: HeadersAllowedAsAttributes = this.headersAllowedAsAttributes,
      perRequestTracingFilter: PerRequestTracingFilter = this.perRequestTracingFilter,
  ): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      this.redactor,
      spanDataProvider,
      routeClassifier,
      headersAllowedAsAttributes,
      perRequestTracingFilter,
    )

  /** Sets how a span's name and `Attributes` are derived from a request and
    * response.
    */
  def withSpanDataProvider(spanDataProvider: SpanDataProvider): ServerMiddlewareBuilder[F] =
    copy(spanDataProvider = spanDataProvider)

  /** Sets how to determine the route within the application from a request. */
  def withRouteClassifier(routeClassifier: RouteClassifier): ServerMiddlewareBuilder[F] =
    copy(routeClassifier = routeClassifier)

  /** Sets which headers are allowed to be made into `Attribute`s. */
  def withHeadersAllowedAsAttributes(
      headersAllowedAsAttributes: HeadersAllowedAsAttributes
  ): ServerMiddlewareBuilder[F] =
    copy(headersAllowedAsAttributes = headersAllowedAsAttributes)

  /** Sets a filter that determines whether each request and its response
    * should be traced.
    */
  def withPerRequestTracingFilter(
      perRequestTracingFilter: PerRequestTracingFilter
  ): ServerMiddlewareBuilder[F] =
    copy(perRequestTracingFilter = perRequestTracingFilter)

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
        !perRequestTracingFilter(req.requestPrelude).isEnabled ||
        !Tracer[F].meta.isEnabled
      ) {
        f(req)
      } else {
        val reqNoBody = req.withBodyStream(Stream.empty)
        val shared =
          spanDataProvider.processSharedData(reqNoBody, routeClassifier, redactor)
        val spanName =
          spanDataProvider.spanName(reqNoBody, routeClassifier, redactor, shared)
        val reqAttributes =
          spanDataProvider.requestAttributes(
            reqNoBody,
            routeClassifier,
            redactor,
            shared,
            headersAllowedAsAttributes.request,
          )
        MonadCancelThrow[G].uncancelable { poll =>
          val tracerG = Tracer[F].mapK[G]
          tracerG.joinOrRoot(req.headers) {
            tracerG
              .spanBuilder(spanName)
              .withSpanKind(SpanKind.Server)
              .addAttributes(reqAttributes)
              .build
              .use { span =>
                poll(f.run(req))
                  .guaranteeCase {
                    case Outcome.Succeeded(fa) =>
                      fa.flatMap { resp =>
                        val respAttributes =
                          spanDataProvider.responseAttributes(
                            resp.withBodyStream(Stream.empty),
                            headersAllowedAsAttributes.response,
                          )
                        span.addAttributes(respAttributes) >> span
                          .setStatus(StatusCode.Error)
                          .unlessA(resp.status.isSuccess)
                      }
                    case Outcome.Errored(e) =>
                      span.addAttributes(spanDataProvider.exceptionAttributes(e))
                    case Outcome.Canceled() =>
                      MonadCancelThrow[G].unit
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

object ServerMiddlewareBuilder {

  /** @return a server middleware builder with default configuration */
  def default[F[_]: Tracer: MonadCancelThrow](
      redactor: PathAndQueryRedactor
  ): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      redactor,
      Defaults.spanDataProvider,
      Defaults.routeClassifier,
      Defaults.headersAllowedAsAttributes,
      Defaults.perRequestTracingFilter,
    )

  /** The default configuration values for a server middleware builder. */
  object Defaults {
    def spanDataProvider: SpanDataProvider = SpanDataProvider.default
    val routeClassifier: RouteClassifier = RouteClassifier.indeterminate
    def headersAllowedAsAttributes: HeadersAllowedAsAttributes =
      HeadersAllowedAsAttributes.default
    def perRequestTracingFilter: PerRequestTracingFilter =
      PerRequestTracingFilter.default
  }
}
