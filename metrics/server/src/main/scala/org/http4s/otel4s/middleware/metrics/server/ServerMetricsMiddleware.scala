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

import cats.effect.Outcome
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.syntax.all._
import cats.syntax.all._
import fs2.Stream
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.ResponsePrelude
import org.typelevel.otel4s.Otel4s

import scala.concurrent.duration.FiniteDuration

private[server] object ServerMetricsMiddleware {

  def create[F[_]: Temporal](
      otel4s: Otel4s[F],
      config: ServerMetricsConfig,
  ): F[HttpApp[F] => HttpApp[F]] =
    ServerMetricsRecorder.create(otel4s, config).map(recorder => wrap(recorder, config))

  def wrap[F[_]: Temporal](
      recorder: ServerMetricsRecorder[F],
      config: ServerMetricsConfig,
  ): HttpApp[F] => HttpApp[F] = app => {
    def metered(request: Request[F], prelude: RequestPrelude): F[Response[F]] =
      Temporal[F].uncancelable { poll =>
        for {
          started <- Temporal[F].monotonic
          claimed <- Ref.of[F, Boolean](false)
          recording <- recorder.start(request, prelude)
          lifecycle = new Lifecycle(started, claimed, recording)
          response <- poll(app(recording.observeRequest(request))).guaranteeCase {
            case Outcome.Errored(error) => lifecycle.finish(None, Some(error))
            case Outcome.Canceled() => lifecycle.finish(None, None)
            case Outcome.Succeeded(_) => Temporal[F].unit
          }
        } yield response.withBodyStream(lifecycle.observe(response))
      }

    HttpApp[F] { request =>
      recorder.isEnabled.ifM(
        {
          val prelude = request.requestPrelude
          if (config.requestFilter(prelude).isEnabled) metered(request, prelude)
          else app(request)
        },
        app(request),
      )
    }
  }

  /** The first app/body completion owns recording. An unstarted body cannot claim completion. */
  private final class Lifecycle[F[_]: Temporal](
      started: FiniteDuration,
      claimed: Ref[F, Boolean],
      recording: ServerMetricsRecorder.Exchange[F],
  ) {
    def finish(response: Option[ResponsePrelude], error: Option[Throwable]): F[Unit] =
      Temporal[F].uncancelable { _ =>
        claimed
          .getAndSet(true)
          .ifM(
            Temporal[F].unit,
            Temporal[F].monotonic
              .flatMap(ended => recording.record(ended - started, response, error)),
          )
      }

    def observe(response: Response[F]): Stream[F, Byte] = {
      val prelude = response.responsePrelude
      val body = recording.observeResponse(response)
      Stream.eval(Ref.of[F, Option[Throwable]](None)).flatMap { error =>
        body
          .translate(recording.runInContext)
          .scope
          .handleErrorWith { cause =>
            Stream.exec(error.set(Some(cause))) ++ Stream.raiseError[F](cause)
          }
          .onFinalize(error.get.flatMap(cause => finish(Some(prelude), cause)))
      }
    }
  }
}
