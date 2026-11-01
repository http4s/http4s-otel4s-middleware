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
import cats.effect.Ref
import cats.syntax.all._
import fs2.Stream

/** Measures the first body evaluation only when normal EOF is observed. */
private[metrics] final class BodySizeObserver[F[_]] private (
    state: Ref[F, BodySizeObserver.State]
) {
  import BodySizeObserver._

  def observe(body: Stream[F, Byte]): Stream[F, Byte] =
    Stream
      .eval(state.modify {
        case Unstarted => (Running, true)
        case current => (current, false)
      })
      .flatMap {
        case false => body
        case true =>
          Stream.suspend {
            var bytes = 0L
            val complete = Stream.exec(state.update {
              case Running => Complete(bytes)
              case current => current
            })
            body.mapChunks { chunk =>
              bytes += chunk.size.toLong
              chunk
            }.scope ++ complete
          }
      }

  /** Freezes the result at exchange completion; later evaluations cannot change it. */
  def freeze: F[Option[Long]] = state.modify {
    case Complete(size) => (Frozen(Some(size)), Some(size))
    case frozen @ Frozen(size) => (frozen, size)
    case _ => (Frozen(None), None)
  }
}

private[metrics] object BodySizeObserver {
  def create[F[_]: Concurrent]: F[BodySizeObserver[F]] =
    Ref.of[F, State](Unstarted).map(new BodySizeObserver(_))

  private sealed trait State
  private case object Unstarted extends State
  private case object Running extends State
  private final case class Complete(bytes: Long) extends State
  private final case class Frozen(bytes: Option[Long]) extends State
}
