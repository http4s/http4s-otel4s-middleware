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

package org.http4s.otel4s.middleware.metrics

import cats.effect.Concurrent
import cats.syntax.all._
import fs2.Stream
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Status
import org.http4s.headers.`Content-Length`
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.Histogram

/** Per-exchange body measurement and its enabled histogram. */
private[metrics] final class BodySizeRecording[F[_]: Concurrent] private (
    histogram: Histogram[F, Long],
    observer: Option[BodySizeObserver[F]],
    declaredSize: Option[Long],
) {
  def contentLength(headers: Headers): Option[Long] =
    if (observer.isEmpty) headers.get[`Content-Length`].map(_.length) else None

  // None means the original message and body can be preserved.
  def observe(body: Stream[F, Byte]): Option[Stream[F, Byte]] = observer.map(_.observe(body))

  def freeze(knownSize: Option[Long] = declaredSize): F[Option[Long]] =
    observer.fold(knownSize.pure[F])(_.freeze.map(knownSize.orElse(_)))

  def record(size: Option[Long], attributes: Attributes): F[Unit] =
    size.traverse_(histogram.record(_, attributes))
}

private[metrics] object BodySizeRecording {

  // These responses have a known zero size regardless of headers or body evaluation.
  def noResponseBody(method: Method, status: Status): Boolean =
    method == Method.HEAD || !status.isEntityAllowed ||
      status.responseClass == Status.Informational ||
      (method == Method.CONNECT && status.responseClass == Status.Successful)

  def create[F[_]: Concurrent](
      config: Option[BodySizeConfig],
      histogram: Option[Histogram[F, Long]],
      headers: Headers = Headers.empty,
  ): F[Option[BodySizeRecording[F]]] =
    (config, histogram).tupled.fold(none[BodySizeRecording[F]].pure[F]) {
      case (settings, instrument) =>
        // Keep this decision for the exchange, so its stream observation and final recording agree.
        instrument.backend.meta.isEnabled.ifM(
          settings.strategy match {
            case BodySizeConfig.Strategy.ContentLength =>
              new BodySizeRecording(
                instrument,
                None,
                headers.get[`Content-Length`].map(_.length),
              ).some.pure[F]
            case BodySizeConfig.Strategy.Observe =>
              BodySizeObserver
                .create[F]
                .map(observer => new BodySizeRecording(instrument, Some(observer), None).some)
          },
          none[BodySizeRecording[F]].pure[F],
        )
    }
}
