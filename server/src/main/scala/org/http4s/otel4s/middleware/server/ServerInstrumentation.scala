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

package org.http4s.otel4s.middleware.server

import cats.effect.Temporal
import cats.syntax.flatMap._
import org.http4s.HttpApp
import org.http4s.otel4s.middleware.BuildInfo
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.semconv.SchemaUrls
import org.typelevel.otel4s.trace.TracerProvider

/** HTTP server metrics and tracing. */
object ServerInstrumentation {

  /** Records metrics before ending the HTTP span when the app fails, is canceled, or the response
    * body finishes, including body cleanup. Apply once to the final app.
    *
    * App and response body effects run in the HTTP span's context, including baggage extracted
    * from request headers. Code consuming the response body keeps its own context. App and body exceptions take
    * precedence over response status errors. Cancellation alone does not count as an HTTP failure.
    *
    * The response body must be run for a successful app response to be recorded. Until then,
    * the span stays open and the request counts as active, including for empty and HEAD responses.
    * Finishing the body does not mean the response reached the client.
    */
  def middleware[F[_]: Temporal: MeterProvider: TracerProvider: ContextManager](
      metricsConfig: ServerMetricsConfig,
      tracingConfig: ServerTracingConfig,
  ): F[HttpApp[F] => HttpApp[F]] =
    TracerProvider[F]
      .tracer("org.http4s.otel4s.middleware.server")
      .withVersion(BuildInfo.version)
      .withSchemaUrl(SchemaUrls.V1_43_0)
      .get
      .flatMap { implicit tracer =>
        ServerInstrumentationMiddleware.create[F](
          metricsConfig,
          tracingConfig,
        )
      }
}
