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

import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Stream
import org.http4s.server.HttpMiddleware
import org.http4s.server.Middleware
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

/** A middleware for wrapping [[org.http4s.HttpApp `HttpApp`]],
  * [[org.http4s.HttpRoutes `HttpRoutes`]], and arbitrary
  * [[org.http4s.Http `Http`]] instances.
  *
  * Middlewares built with [[ServerMiddleware.builder]] add tracing.
  *
  * @note This API requires
  *       [[cats.effect.kernel.MonadCancelThrow `MonadCancelThrow`]] in order
  *       to support tracing, and may be overly constraining for some
  *       middlewares. Consequently, it should not be considered a
  *       general-purpose interface for middlewares, and should be reserved for
  *       middlewares that need to compose with one that adds tracing.
  */
trait ServerMiddleware[F[_]] { self =>

  implicit def monadCancelThrow: MonadCancelThrow[F]

  /** Wraps an [[org.http4s.Http `Http`]] in a way that abstracts over
    * [[org.http4s.HttpApp `HttpApp`]] and
    * [[org.http4s.HttpRoutes `HttpRoutes`]]. In most cases, it is preferable
    * to use the method that directly wraps the specific desired type.
    *
    * @return the given `Http` modified with this middleware's functionality
    * @see [[wrapHttpApp]]
    * @see [[wrapHttpRoutes]]
    */
  def wrapGenericHttp[G[_]: MonadCancelThrow](
      http: Http[G, F]
  )(implicit kt: KindTransformer[F, G]): Http[G, F]

  /** The wrapping of [[org.http4s.Http `Http`]] in a way that abstracts over
    * [[org.http4s.HttpApp `HttpApp`]] and
    * [[org.http4s.HttpRoutes `HttpRoutes`]], expressed as a
    * [[org.http4s.server.Middleware `Middleware`]]. In most cases, it is preferable
    * to use the `Middleware` that directly wraps the specific desired type.
    *
    * @see [[wrapGenericHttp]]
    * @see [[asHttpAppMiddleware]]
    * @see [[asHttpRoutesMiddleware]]
    */
  final def asGenericHttpMiddleware[G[_]: MonadCancelThrow](implicit
      kt: KindTransformer[F, G]
  ): Middleware[G, Request[F], Response[F], Request[F], Response[F]] =
    wrapGenericHttp(_)

  /** @return the given [[org.http4s.HttpApp `HttpApp`]] modified with this
    *         middleware's functionality
    */
  final def wrapHttpApp(httpApp: HttpApp[F]): HttpApp[F] =
    wrapGenericHttp(httpApp)

  /** The wrapping of [[org.http4s.HttpApp `HttpApp`s]] expressed as a
    * [[org.http4s.server.Middleware `Middleware`]].
    *
    * @see [[wrapHttpApp]]
    */
  final def asHttpAppMiddleware: Middleware[F, Request[F], Response[F], Request[F], Response[F]] =
    wrapHttpApp

  /** @return the given [[org.http4s.HttpRoutes `HttpRoutes`]] modified with
    *         this middleware's functionality
    */
  final def wrapHttpRoutes(httpRoutes: HttpRoutes[F]): HttpRoutes[F] =
    wrapGenericHttp(httpRoutes)

  /** The wrapping of [[org.http4s.HttpRoutes `HttpRoutes`]] expressed as an
    * [[org.http4s.server.HttpMiddleware `HttpMiddleware`]].
    *
    * @see [[wrapHttpRoutes]]
    */
  final def asHttpRoutesMiddleware: HttpMiddleware[F] =
    wrapHttpRoutes

  /** @return a middleware that modifies [[org.http4s.HttpApp `HttpApp`]],
    *         [[org.http4s.HttpRoutes `HttpRoutes`]], and arbitrary
    *         [[org.http4s.Http `Http`]] instances using `that` middleware,
    *         and the resulting instance using this middleware
    */
  final def wrapMiddleware(that: ServerMiddleware[F]): ServerMiddleware[F] =
    new ServerMiddleware[F] {
      implicit def monadCancelThrow: MonadCancelThrow[F] =
        self.monadCancelThrow
      def wrapGenericHttp[G[_]: MonadCancelThrow](http: Http[G, F])(implicit
          kt: KindTransformer[F, G]
      ): Http[G, F] = self.wrapGenericHttp(that.wrapGenericHttp(http))
    }
}

object ServerMiddleware {
  private[this] final class Impl[F[_]](
      tracerF: Tracer[F],
      spanDataProvider: SpanDataProvider,
      perRequestReversePropagationFilter: PerRequestFilter,
      perRequestTracingFilter: PerRequestFilter,
  )(implicit val monadCancelThrow: MonadCancelThrow[F])
      extends ServerMiddleware[F] {
    def wrapGenericHttp[G[_]](http: Http[G, F])(implicit
        G: MonadCancelThrow[G],
        kt: KindTransformer[F, G],
    ): Http[G, F] = Http[G, F] { req =>
      tracerF.liftTo[G].meta.isEnabled.flatMap { tracerEnabled =>
        val reqPrelude = req.requestPrelude
        if (
          !tracerEnabled ||
          !perRequestTracingFilter(reqPrelude).isEnabled
        ) {
          http.run(req)
        } else {
          val reqNoBody = req.withBodyStream(Stream.empty)
          val shared = spanDataProvider.processSharedData(reqNoBody)
          val spanName = spanDataProvider.spanName(reqNoBody, shared)
          val reqAttributes = spanDataProvider.requestAttributes(reqNoBody, shared)
          G.uncancelable { poll =>
            val tracerG = tracerF.liftTo[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG
                .spanBuilder(spanName)
                .withSpanKind(SpanKind.Server)
                .addAttributes(reqAttributes)
                .build
                .use { span =>
                  poll {
                    http.run(req).flatMap { resp =>
                      if (perRequestReversePropagationFilter(reqPrelude).isEnabled) {
                        for (traceHeaders <- tracerG.propagate(Headers.empty))
                          yield resp.withHeaders(resp.headers ++ traceHeaders)
                      } else G.pure(resp)
                    }
                  }.guaranteeCase {
                    case Outcome.Succeeded(fa) =>
                      fa.flatMap { resp =>
                        val respAttributes =
                          spanDataProvider.responseAttributes(resp.withBodyStream(Stream.empty))
                        span.addAttributes(respAttributes) >> span
                          .setStatus(StatusCode.Error)
                          .whenA(resp.status.responseClass == Status.ServerError)
                      }
                    case Outcome.Errored(e) =>
                      span.addAttributes(spanDataProvider.exceptionAttributes(e))
                    case Outcome.Canceled() =>
                      G.unit
                  }
                }
            }
          }
        }
      }
    }
  }

  /** A builder for [[`ServerMiddleware`]]s that add tracing. */
  final class Builder[F[_]: MonadCancelThrow] private[ServerMiddleware] (
      spanDataProvider: SpanDataProvider,
      perRequestReversePropagationFilter: PerRequestFilter,
      perRequestTracingFilter: PerRequestFilter,
  )(implicit tracerProvider: TracerProvider[F]) {
    private[this] def copy(
        perRequestReversePropagationFilter: PerRequestFilter =
          this.perRequestReversePropagationFilter,
        perRequestTracingFilter: PerRequestFilter = this.perRequestTracingFilter,
    ): Builder[F] =
      new Builder(
        spanDataProvider = this.spanDataProvider,
        perRequestReversePropagationFilter = perRequestReversePropagationFilter,
        perRequestTracingFilter = perRequestTracingFilter,
      )

    /** Sets a filter that determines whether each request should propagate
      * tracing and other context information back to the requesting client in
      * the response headers, primarily for debugging (default: never enabled).
      */
    def withPerRequestReversePropagationFilter(
        perRequestReversePropagationFilter: PerRequestFilter
    ): Builder[F] =
      copy(perRequestReversePropagationFilter = perRequestReversePropagationFilter)

    /** Sets a filter that determines whether each request and its response
      * should be traced (default: always enabled).
      */
    def withPerRequestTracingFilter(
        perRequestTracingFilter: PerRequestFilter
    ): Builder[F] =
      copy(perRequestTracingFilter = perRequestTracingFilter)

    /** @return a middleware that can wrap [[org.http4s.HttpApp `HttpApp`]],
      *         [[org.http4s.HttpRoutes `HttpRoutes`]], and arbitrary
      *         [[org.http4s.Http `Http`]] instances to add tracing
      */
    def build: F[ServerMiddleware[F]] =
      for {
        tracer <- tracerProvider
          .tracer("org.http4s.otel4s.middleware.server")
          .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
          .withSchemaUrl("https://opentelemetry.io/schemas/1.30.0")
          .get
      } yield new Impl(
        tracerF = tracer,
        spanDataProvider = spanDataProvider,
        perRequestReversePropagationFilter = perRequestReversePropagationFilter,
        perRequestTracingFilter = perRequestTracingFilter,
      )
  }

  /** @return a [[`Builder`]] that uses the given [[`SpanDataProvider`]]
    * @see [[ServerSpanDataProvider.openTelemetry]] for creating OpenTelemetry-
    *      compliant providers
    */
  def builder[F[_]: MonadCancelThrow: TracerProvider](
      spanDataProvider: SpanDataProvider
  ): Builder[F] =
    new Builder[F](
      spanDataProvider = spanDataProvider,
      perRequestReversePropagationFilter = PerRequestFilter.neverEnabled,
      perRequestTracingFilter = PerRequestFilter.alwaysEnabled,
    )
}
