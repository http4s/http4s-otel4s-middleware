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

package org.http4s.otel4s.middleware.metrics.client

import cats.effect.Temporal
import org.http4s.client.Client
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider

/** Client metrics entry points. */
object ClientMetrics {

  /** Records duration, and optionally active requests, without creating spans.
    *
    * Completes at body termination or unread-response release, before backend cleanup.
    * Observed acquisition/body errors take precedence over error response status.
    * Recording restores acquisition context. For HTTP-span exemplars, place tracing outside metrics.
    * Supply the meter provider and context manager from the same telemetry instance.
    */
  def middleware[F[_]: Temporal: MeterProvider: ContextManager](
      config: ClientMetricsConfig
  ): F[Client[F] => Client[F]] =
    ClientMetricsMiddleware.create(config)

}
