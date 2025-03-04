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

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Outcome
import cats.effect.Resource
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Stream
import org.http4s.client.Client
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.TracerProvider

/** Middleware builder for wrapping an http4s `Client` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client]]
  */
class ClientMiddlewareBuilder[F[_]: TracerProvider: Concurrent] private (
    spanDataProvider: SpanDataProvider,
    perRequestTracingFilter: PerRequestTracingFilter,
) {
  private[this] def copy(
      perRequestTracingFilter: PerRequestTracingFilter
  ): ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](
      this.spanDataProvider,
      perRequestTracingFilter,
    )

  /** Sets a filter that determines whether each request and its response
    * should be traced.
    */
  def withPerRequestTracingFilter(
      perRequestTracingFilter: PerRequestTracingFilter
  ): ClientMiddlewareBuilder[F] =
    copy(perRequestTracingFilter = perRequestTracingFilter)

  /** @return the configured middleware */
  def build: F[Client[F] => Client[F]] =
    for {
      tracer <- TracerProvider[F]
        .tracer("org.http4s.otel4s.middleware.client")
        .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
        .withSchemaUrl("https://opentelemetry.io/schemas/1.30.0")
        .get
    } yield (client: Client[F]) =>
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]
        if (
          !perRequestTracingFilter(req.requestPrelude).isEnabled ||
          !tracer.meta.isEnabled
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
              traceHeaders <- Resource.eval(tracer.propagate(Headers.empty)).mapK(trace)
              newReq = req.withHeaders(traceHeaders ++ req.headers)

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

object ClientMiddlewareBuilder {

  /** @return a client middleware builder that uses the given [[`SpanDataProvider`]]
    * @see [[ClientSpanDataProvider]] for creating an OpenTelemetry-compliant provider
    */
  def apply[F[_]: TracerProvider: Concurrent](
      spanDataProvider: SpanDataProvider
  ): ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](spanDataProvider, PerRequestTracingFilter.default)
}
