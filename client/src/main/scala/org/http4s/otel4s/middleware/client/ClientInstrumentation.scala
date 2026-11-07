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

package org.http4s.otel4s.middleware.client

import cats.effect.Temporal
import cats.syntax.flatMap._
import org.http4s.client.Client
import org.http4s.otel4s.middleware.BuildInfo
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import org.typelevel.otel4s.semconv.SchemaUrls

/** HTTP client metrics and tracing. */
object ClientInstrumentation {

  /** Records metrics and ends the HTTP span when the response body finishes or the response
    * resource is released, before the client's release finalizers run.
    *
    * Request and response body effects run in the HTTP span's context. Code consuming the response
    * body keeps its own context.
    *
    * Request and response body exceptions take precedence over response status errors.
    * Cancellation and errors in caller code do not count as HTTP failures.
    * Callers must release the response resource for unread responses to be recorded.
    */
  def middleware[F[_]: Temporal: MeterProvider: TracerProvider: ContextManager](
      metricsConfig: ClientMetricsConfig,
      tracingConfig: ClientTracingConfig,
  ): F[Client[F] => Client[F]] =
    TracerProvider[F]
      .tracer("org.http4s.otel4s.middleware.client")
      .withVersion(BuildInfo.version)
      .withSchemaUrl(SchemaUrls.V1_43_0)
      .get
      .flatMap { implicit tracer =>
        ClientInstrumentationMiddleware.create[F](
          metricsConfig,
          tracingConfig,
        )
      }
}
