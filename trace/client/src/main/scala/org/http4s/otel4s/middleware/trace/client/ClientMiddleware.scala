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
package client

import cats.effect.MonadCancelThrow
import cats.effect.Outcome
import cats.effect.Resource
import cats.effect.syntax.resource._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Stream
import org.http4s.client.Client
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

/** Middleware for wrapping [[org.http4s.client.Client HTTP `Client`s]] to add
  * tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client]]
  */
sealed trait ClientMiddleware[F[_]] {

  /** @return a traced wrapper around the given
    *         [[org.http4s.client.Client `Client`]]
    */
  def wrap(client: Client[F]): Client[F]
}

object ClientMiddleware {
  private[this] class Impl[F[_]: MonadCancelThrow](
      tracer: Tracer[F],
      spanDataProvider: SpanDataProvider,
      perRequestPropagationFilter: PerRequestFilter,
      perRequestTracingFilter: PerRequestFilter,
  ) extends ClientMiddleware[F] {
    def wrap(client: Client[F]): Client[F] =
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]
        tracer.meta.isEnabled.toResource.flatMap { tracerEnabled =>
          val reqPrelude = req.requestPrelude
          if (
            !tracerEnabled ||
            !perRequestTracingFilter(reqPrelude).isEnabled
          ) {
            client.run(req)
          } else {
            val reqNoBody = req.withBodyStream(Stream.empty)
            val shared = spanDataProvider.processSharedData(reqNoBody)
            val spanName = spanDataProvider.spanName(reqNoBody, shared)
            val reqAttributes = spanDataProvider.requestAttributes(reqNoBody, shared)

            MonadCancelThrow[Resource[F, *]].uncancelable { poll =>
              for {
                res <- tracer
                  .spanBuilder(spanName)
                  .withSpanKind(SpanKind.Client)
                  .addAttributes(reqAttributes)
                  .build
                  .resource
                span = res.span
                trace = res.trace
                traceHeaders <-
                  if (perRequestPropagationFilter(reqPrelude).isEnabled) {
                    // propagate onto empty headers because propagators can
                    // theoretically erase all the headers, even though they
                    // really shouldn't
                    Resource.eval(tracer.propagate(Headers.empty)).mapK(trace)
                  } else Resource.pure[F, Headers](Headers.empty)
                newReq = req.withHeaders(req.headers ++ traceHeaders)

                resp <- poll(client.run(newReq).mapK(trace)).guaranteeCase {
                  case Outcome.Succeeded(fa) =>
                    fa.evalMap { resp =>
                      val respAttributes =
                        spanDataProvider.responseAttributes(resp.withBodyStream(Stream.empty))
                      span.addAttributes(respAttributes) >> span
                        .setStatus(StatusCode.Error)
                        .unlessA(resp.status.isSuccess)
                    }

                  case Outcome.Errored(e) =>
                    Resource.eval {
                      span.addAttributes(spanDataProvider.exceptionAttributes(e))
                    }

                  case Outcome.Canceled() =>
                    Resource.unit
                }
              } yield resp
            }
          }
        }
      }
  }

  /** A builder for [[`ClientMiddleware`]]s. */
  final class Builder[F[_]: MonadCancelThrow] private[ClientMiddleware] (
      spanDataProvider: SpanDataProvider,
      perRequestPropagationFilter: PerRequestFilter,
      perRequestTracingFilter: PerRequestFilter,
  )(implicit tracerProvider: TracerProvider[F]) {
    private[this] def copy(
        perRequestPropagationFilter: PerRequestFilter = this.perRequestPropagationFilter,
        perRequestTracingFilter: PerRequestFilter = this.perRequestTracingFilter,
    ): Builder[F] =
      new Builder(
        spanDataProvider = this.spanDataProvider,
        perRequestPropagationFilter = perRequestPropagationFilter,
        perRequestTracingFilter = perRequestTracingFilter,
      )

    /** Sets a filter that determines whether each request should propagate
      * tracing and other context information to the server.
      */
    def withPerRequestPropagationFilter(
        perRequestPropagationFilter: PerRequestFilter
    ): Builder[F] =
      copy(perRequestPropagationFilter = perRequestPropagationFilter)

    /** Sets a filter that determines whether each request and its response
      * should be traced.
      */
    def withPerRequestTracingFilter(
        perRequestTracingFilter: PerRequestFilter
    ): Builder[F] =
      copy(perRequestTracingFilter = perRequestTracingFilter)

    /** @return a middleware that can wrap
      *         [[org.http4s.client.Client `Client`s]] to add tracing
      */
    def build: F[ClientMiddleware[F]] =
      for {
        tracer <- tracerProvider
          .tracer("org.http4s.otel4s.middleware.client")
          .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
          .withSchemaUrl("https://opentelemetry.io/schemas/1.30.0")
          .get
      } yield new Impl(
        tracer = tracer,
        spanDataProvider = spanDataProvider,
        perRequestPropagationFilter = perRequestPropagationFilter,
        perRequestTracingFilter = perRequestTracingFilter,
      )
  }

  /** @return a [[`Builder`]] that uses the given [[`SpanDataProvider`]]
    * @see [[`ClientSpanDataProvider`]] for creating an OpenTelemetry-compliant provider
    */
  def builder[F[_]: MonadCancelThrow: TracerProvider](
      spanDataProvider: SpanDataProvider
  ): Builder[F] =
    new Builder(
      spanDataProvider = spanDataProvider,
      perRequestPropagationFilter = PerRequestFilter.alwaysEnabled,
      perRequestTracingFilter = PerRequestFilter.alwaysEnabled,
    )
}
