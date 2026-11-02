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
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite

import scala.concurrent.duration._

class ExchangeCompletionTest extends CatsEffectSuite {
  test(
    "first completion owns one outcome and timestamp; concurrent callers await recording and end"
  ) {
    TestControl.executeEmbed {
      val firstError = new IllegalStateException("source")
      val laterError = new IllegalArgumentException("late consumer")
      for {
        started <- ExchangeCompletion.started[IO]
        records <- Ref.of[IO, Vector[ExchangeCompletion.Terminal]](Vector.empty)
        ends <- Ref.of[IO, Vector[ExchangeCompletion.Terminal]](Vector.empty)
        entered <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        loserDone <- Deferred[IO, Unit]
        completion <- ExchangeCompletion.create[IO](
          started,
          terminal => records.update(_ :+ terminal) >> entered.complete(()).void >> release.get,
          terminal => IO.sleep(7.seconds) >> ends.update(_ :+ terminal),
        )
        _ <- IO.sleep(2.seconds)
        winner <- completion.finish(Some(firstError)).start
        _ <- entered.get
        loser <- completion.finish(Some(laterError)).guarantee(loserDone.complete(()).void).start
        _ <- IO.sleep(1.second)
        pending <- loserDone.tryGet
        before <- ends.get
        _ <- release.complete(())
        _ <- winner.joinWithNever
        _ <- loser.joinWithNever
        _ <- completion.finish(None)
        recorded <- records.get
        ended <- ends.get
      } yield {
        assertEquals(pending, None)
        assertEquals(before, Vector.empty)
        assertEquals(recorded.size, 1)
        assertEquals(recorded.head.error, Some(firstError))
        assertEquals(recorded.head.elapsed, 2.seconds)
        assertEquals(recorded.head.timestamp, started.timestamp + 2.seconds)
        assertEquals(ended, recorded)
      }
    }
  }

  List("record", "end").foreach { phase =>
    test(s"a failed $phase callback completes waiters and is not retried") {
      val boom = new IllegalStateException("telemetry")
      for {
        started <- ExchangeCompletion.started[IO]
        calls <- Ref.of[IO, Vector[String]](Vector.empty)
        entered <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        completion <- ExchangeCompletion.create[IO](
          started,
          _ =>
            calls.update(_ :+ "record") >> entered.complete(()).void >> release.get >>
              (if (phase == "record") IO.raiseError[Unit](boom) else IO.unit),
          _ =>
            calls.update(_ :+ "end") >> (if (phase == "end") IO.raiseError[Unit](boom) else IO.unit),
        )
        winner <- completion.finish(None).attempt.start
        _ <- entered.get
        loser <- completion.finish(Some(new RuntimeException("ignored"))).attempt.start
        _ <- release.complete(())
        first <- winner.joinWithNever
        second <- loser.joinWithNever
        third <- completion.finish(None).attempt
        steps <- calls.get
      } yield {
        assertEquals(first, Left(boom))
        assertEquals(second, first)
        assertEquals(third, first)
        assertEquals(steps, Vector("record", "end"))
      }
    }
  }

  test("a throwing record callback still ends the span and completes future callers") {
    val boom = new IllegalStateException("callback construction")
    for {
      started <- ExchangeCompletion.started[IO]
      ends <- Ref.of[IO, Int](0)
      completion <- ExchangeCompletion.create[IO](started, _ => throw boom, _ => ends.update(_ + 1))
      first <- completion.finish(None).attempt
      second <- completion.finish(None).attempt
      count <- ends.get
    } yield {
      assertEquals(first, Left(boom))
      assertEquals(second, first)
      assertEquals(count, 1)
    }
  }

  test("cancellation cannot interrupt the winning commit or leave waiters blocked") {
    for {
      started <- ExchangeCompletion.started[IO]
      entered <- Deferred[IO, Unit]
      release <- Deferred[IO, Unit]
      calls <- Ref.of[IO, Vector[String]](Vector.empty)
      completion <- ExchangeCompletion.create[IO](
        started,
        _ => calls.update(_ :+ "record") >> entered.complete(()).void >> release.get,
        _ => calls.update(_ :+ "end"),
      )
      winner <- completion.finish(None).start
      _ <- entered.get
      cancel <- winner.cancel.start
      _ <- release.complete(())
      _ <- cancel.joinWithNever
      _ <- completion.finish(None)
      steps <- calls.get
    } yield assertEquals(steps, Vector("record", "end"))
  }
}
