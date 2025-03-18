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
import cats.syntax.functor._
import fs2.Stream
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

/** Middleware for wrapping [[org.http4s.HttpApp `HttpApp`]],
  * [[org.http4s.HttpRoutes `HttpRoutes`]], and arbitrary
  * [[org.http4s.Http `Http`]] instances to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  */
sealed trait ServerMiddleware[F[_]] {

  /** Wraps an [[org.http4s.Http `Http`]] to add tracing in a way that
    * abstracts over [[org.http4s.HttpApp `HttpApp`]] and
    * [[org.http4s.HttpRoutes `HttpRoutes`]]. In most cases, it is preferable
    * to use the methods that directly wrap the specific desired type.
    *
    * @return a traced wrapper around the given `Http`
    * @see [[wrapHttpApp]]
    * @see [[wrapHttpRoutes]]
    */
  def wrapGenericHttp[G[_]: MonadCancelThrow](
      f: Http[G, F]
  )(implicit kt: KindTransformer[F, G]): Http[G, F]

  /** @return a traced wrapper around the given
    *         [[org.http4s.HttpApp `HttpApp`]]
    */
  def wrapHttpApp(httpApp: HttpApp[F]): HttpApp[F]

  /** @return a traced wrapper around the given
    *         [[org.http4s.HttpRoutes `HttpRoutes`]]
    */
  def wrapHttpRoutes(httpRoutes: HttpRoutes[F]): HttpRoutes[F]
}

object ServerMiddleware {
  private[this] final class Impl[F[_]: MonadCancelThrow](
      tracerF: Tracer[F],
      spanDataProvider: SpanDataProvider,
      perRequestTracingFilter: PerRequestTracingFilter,
  ) extends ServerMiddleware[F] {
    def wrapGenericHttp[G[_]: MonadCancelThrow](f: Http[G, F])(implicit
        kt: KindTransformer[F, G]
    ): Http[G, F] = Kleisli { (req: Request[F]) =>
      if (
        !perRequestTracingFilter(req.requestPrelude).isEnabled ||
        !tracerF.meta.isEnabled
      ) {
        f(req)
      } else {
        val reqNoBody = req.withBodyStream(Stream.empty)
        val shared = spanDataProvider.processSharedData(reqNoBody)
        val spanName = spanDataProvider.spanName(reqNoBody, shared)
        val reqAttributes = spanDataProvider.requestAttributes(reqNoBody, shared)
        MonadCancelThrow[G].uncancelable { poll =>
          val tracerG = tracerF.mapK[G]
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
                          spanDataProvider.responseAttributes(resp.withBodyStream(Stream.empty))
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
    def wrapHttpApp(httpApp: HttpApp[F]): HttpApp[F] =
      wrapGenericHttp(httpApp)
    def wrapHttpRoutes(httpRoutes: HttpRoutes[F]): HttpRoutes[F] =
      wrapGenericHttp(httpRoutes)
  }

  /** A builder for [[`ServerMiddleware`]]s. */
  final class Builder[F[_]: MonadCancelThrow] private[ServerMiddleware] (
      spanDataProvider: SpanDataProvider,
      perRequestTracingFilter: PerRequestTracingFilter,
  )(implicit tracerProvider: TracerProvider[F]) {
    // in case the builder ever gets more parameters
    private[this] def copy(
        perRequestTracingFilter: PerRequestTracingFilter
    ): Builder[F] =
      new Builder(
        this.spanDataProvider,
        perRequestTracingFilter,
      )

    /** Sets a filter that determines whether each request and its response
      * should be traced.
      */
    def withPerRequestTracingFilter(
        perRequestTracingFilter: PerRequestTracingFilter
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
      } yield new Impl(tracer, spanDataProvider, perRequestTracingFilter)
  }

  /** @return a [[`Builder`]] that uses the given [[`SpanDataProvider`]]
    * @see [[`ServerSpanDataProvider`]] for creating an OpenTelemetry-compliant provider
    */
  def builder[F[_]: MonadCancelThrow: TracerProvider](
      spanDataProvider: SpanDataProvider
  ): Builder[F] =
    new Builder[F](spanDataProvider, PerRequestTracingFilter.alwaysTrace)
}
