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

import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all._
import fs2.Stream
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.client.Client
import org.typelevel.otel4s.Otel4s

import scala.concurrent.duration.FiniteDuration

private[client] object ClientMetricsMiddleware {

  def create[F[_]: Temporal](
      otel4s: Otel4s[F],
      config: ClientMetricsConfig,
  ): F[Client[F] => Client[F]] =
    ClientMetricsRecorder.create(otel4s, config).map(recorder => wrap(recorder, config))

  def wrap[F[_]: Temporal](
      recorder: ClientMetricsRecorder[F],
      config: ClientMetricsConfig,
  ): Client[F] => Client[F] =
    client => {
      def metered(request: Request[F], prelude: RequestPrelude): Resource[F, Response[F]] = {
        val acquire = recorder.start(request, prelude)
        // The outer owner handles acquisition failure/cancellation; the inner fallback runs
        // before backend response-release finalizers.
        Resource
          .makeCase {
            for {
              started <- Temporal[F].monotonic
              claimed <- Ref.of[F, Boolean](false)
              recording <- acquire
            } yield new Lifecycle(started, claimed, recording)
          } { (lifecycle, exit) =>
            lifecycle.recording.hasResponse.ifM(
              Temporal[F].unit,
              lifecycle.finish(exit match {
                case Resource.ExitCase.Errored(error) => Some(error)
                case _ => None
              }),
            )
          }
          .flatMap { lifecycle =>
            client.run(lifecycle.recording.observeRequest(request)).flatMap { response =>
              Resource.make(
                lifecycle.recording
                  .observeResponse(prelude, response)
                  .map(observed => observed.withBodyStream(lifecycle.observe(observed.body)))
              )(_ => lifecycle.finish(None))
            }
          }
      }
      Client[F] { request =>
        Resource.eval(recorder.isEnabled).flatMap { enabled =>
          if (enabled) {
            val prelude = request.requestPrelude
            if (config.requestFilter(prelude).isEnabled) metered(request, prelude)
            else client.run(request)
          } else client.run(request)
        }
      }
    }

  /** Owns completion for one exchange. Body termination and response release may both attempt
    * completion; the atomic claim makes the first attempt the only recorder. The outer resource
    * handles acquisition failure/cancellation only while no response has been observed.
    */
  private final class Lifecycle[F[_]: Temporal](
      started: FiniteDuration,
      claimed: Ref[F, Boolean],
      val recording: ClientMetricsRecorder.Exchange[F],
  ) {
    def finish(error: Option[Throwable]): F[Unit] = Temporal[F].uncancelable { _ =>
      claimed
        .getAndSet(true)
        .ifM(
          Temporal[F].unit,
          Temporal[F].monotonic.flatMap(ended => recording.record(ended - started, error)),
        )
    }

    def observe(body: Stream[F, Byte]): Stream[F, Byte] =
      Stream.eval(Ref.of[F, Option[Throwable]](None)).flatMap { error =>
        body.scope
          .handleErrorWith { cause =>
            Stream.exec(error.set(Some(cause))) ++ Stream.raiseError[F](cause)
          }
          .onFinalize(error.get.flatMap(finish))
      }
  }
}
