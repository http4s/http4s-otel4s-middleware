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
import org.typelevel.otel4s.trace.TracerProvider

/** Middleware builder for wrapping an http4s `Server` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  */
class ServerMiddlewareBuilder[F[_]: TracerProvider: MonadCancelThrow] private (
    spanDataProvider: SpanDataProvider,
    perRequestTracingFilter: PerRequestTracingFilter,
) {
  private[this] def copy(
      perRequestTracingFilter: PerRequestTracingFilter
  ): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      this.spanDataProvider,
      perRequestTracingFilter,
    )

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
  )(implicit kt: KindTransformer[F, G]): F[Http[G, F]] =
    for {
      tracerF <- TracerProvider[F]
        .tracer("org.http4s.otel4s.middleware.server")
        .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
        .withSchemaUrl("https://opentelemetry.io/schemas/1.30.0")
        .get
    } yield Kleisli { (req: Request[F]) =>
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

  /** @return a configured middleware for `HttpApp` */
  def buildHttpApp(f: HttpApp[F]): F[HttpApp[F]] =
    buildGenericTracedHttp(f)

  /** @return a configured middleware for `HttpRoutes` */
  def buildHttpRoutes(f: HttpRoutes[F]): F[HttpRoutes[F]] =
    buildGenericTracedHttp(f)
}

object ServerMiddlewareBuilder {

  /** @return a server middleware builder that uses the given [[`SpanDataProvider`]]
    * @see [[ServerSpanDataProvider]] for creating an OpenTelemetry-compliant provider
    */
  def apply[F[_]: TracerProvider: MonadCancelThrow](
      spanDataProvider: SpanDataProvider
  ): ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](spanDataProvider, PerRequestTracingFilter.default)
}
