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

package org.http4s.otel4s.middleware.metrics.server

import cats.effect.Temporal
import org.http4s.HttpApp
import org.typelevel.otel4s.Otel4s

/** Server metrics entry point. */
object ServerMetrics {

  /** Records duration and optional active requests at response-body termination.
    *
    * App failure/cancellation completes immediately. An unstarted response body leaves duration
    * unrecorded and active requests incremented, including empty and HEAD responses.
    * Body effects and recording restore the invocation's full telemetry context.
    * Completion includes body cleanup; it does not establish network delivery.
    */
  def middleware[F[_]: Temporal](
      otel4s: Otel4s[F],
      config: ServerMetricsConfig,
  ): F[HttpApp[F] => HttpApp[F]] = ServerMetricsMiddleware.create(otel4s, config)
}
