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

package org.http4s.otel4s.middleware

import cats.effect.Deferred
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.syntax.all._
import cats.syntax.all._

import scala.concurrent.duration.FiniteDuration

/** One terminal fact and clock snapshot, recorded before finalization. */
private[middleware] final class ExchangeCompletion[F[_]: Temporal] private (
    start: ExchangeCompletion.Started,
    claimed: Ref[F, Boolean],
    done: Deferred[F, Either[Throwable, Unit]],
    record: ExchangeCompletion.Terminal => F[Unit],
    end: ExchangeCompletion.Terminal => F[Unit],
) {

  /** First caller selects the outcome and timestamps; losing calls join its commit. */
  def finish(error: Option[Throwable]): F[Unit] = Temporal[F].uncancelable { _ =>
    claimed
      .getAndSet(true)
      .ifM(
        done.get.rethrow,
        recordAndEnd(error).attempt.flatTap(result => done.complete(result)).rethrow,
      )
  }

  private def recordAndEnd(error: Option[Throwable]): F[Unit] =
    for {
      monotonic <- Temporal[F].monotonic
      timestamp <- Temporal[F].realTime
      terminal = ExchangeCompletion.Terminal(error, monotonic - start.monotonic, timestamp)
      _ <- Temporal[F].unit
        .flatMap(_ => record(terminal))
        .guarantee(Temporal[F].unit.flatMap(_ => end(terminal)))
    } yield ()

}

private[middleware] object ExchangeCompletion {
  final case class Started(monotonic: FiniteDuration, timestamp: FiniteDuration)
  final case class Terminal(
      error: Option[Throwable],
      elapsed: FiniteDuration,
      timestamp: FiniteDuration,
  )

  def started[F[_]: Temporal]: F[Started] =
    (Temporal[F].monotonic, Temporal[F].realTime).mapN(Started.apply)

  def create[F[_]: Temporal](
      start: Started,
      record: Terminal => F[Unit],
      end: Terminal => F[Unit],
  ): F[ExchangeCompletion[F]] =
    (Ref.of[F, Boolean](false), Deferred[F, Either[Throwable, Unit]]).mapN { (claimed, done) =>
      new ExchangeCompletion(start, claimed, done, record, end)
    }
}
