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

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import fs2.Chunk
import fs2.Stream
import munit.CatsEffectSuite

class BodySizeObserverTest extends CatsEffectSuite {
  private val chunks = Vector(Chunk.array(Array[Byte](1, 2, 3)), Chunk.array(Array[Byte](4, 5)))
  private val body = Stream.emits(chunks).flatMap(Stream.chunk(_)).covary[IO]

  test("a full drain preserves chunks and records their total size") {
    for {
      observer <- BodySizeObserver.create[IO]
      observed <- observer.observe(body).chunks.compile.toVector
      size <- observer.freeze
      _ <- observer.observe(body.take(1)).compile.drain
      again <- observer.freeze
    } yield {
      assertEquals(observed, chunks)
      assertEquals(size, Some(5L))
      assertEquals(again, size)
    }
  }

  test("an evaluated empty body has a known zero size") {
    for {
      observer <- BodySizeObserver.create[IO]
      _ <- observer.observe(Stream.empty).compile.drain
      size <- observer.freeze
    } yield assertEquals(size, Some(0L))
  }

  test("an unstarted body stays unknown and does not run effects or cleanup") {
    for {
      observer <- BodySizeObserver.create[IO]
      effects <- Ref.of[IO, Int](0)
      observed = observer.observe(
        Stream.exec(effects.update(_ + 1)).onFinalize(effects.update(_ + 10))
      )
      size <- observer.freeze
      before <- effects.get
      _ <- observed.compile.drain
      after <- effects.get
      frozen <- observer.freeze
    } yield {
      assertEquals(size, None)
      assertEquals(before, 0)
      assertEquals(after, 11)
      assertEquals(frozen, None)
    }
  }

  List(1L, 3L, 5L, 6L).foreach { limit =>
    test(s"take($limit) requires observed EOF and does not pull extra chunks") {
      for {
        observer <- BodySizeObserver.create[IO]
        pulled <- Ref.of[IO, Int](0)
        released <- Ref.of[IO, Int](0)
        source = body.chunks
          .evalTap(_ => pulled.update(_ + 1))
          .unchunks
          .onFinalize(released.update(_ + 1))
        bytes <- observer.observe(source).take(limit).compile.toVector
        size <- observer.freeze
        pulls <- pulled.get
        releases <- released.get
      } yield {
        assertEquals(bytes, Vector[Byte](1, 2, 3, 4, 5).take(limit.toInt))
        assertEquals(size, if (limit > 5L) Some(5L) else None)
        assertEquals(pulls, if (limit <= 3L) 1 else 2)
        assertEquals(releases, 1)
      }
    }
  }

  List(false, true).foreach { cleanupFailure =>
    test(s"an error ${if (cleanupFailure) "in cleanup" else "before EOF"} keeps size unknown") {
      val boom = new RuntimeException("body failure")
      for {
        observer <- BodySizeObserver.create[IO]
        released <- Ref.of[IO, Int](0)
        source =
          if (cleanupFailure) body.onFinalize(IO.raiseError[Unit](boom))
          else body ++ Stream.raiseError[IO](boom)
        result <- observer.observe(source.onFinalize(released.update(_ + 1))).compile.drain.attempt
        size <- observer.freeze
        releases <- released.get
      } yield {
        assertEquals(result, Left(boom))
        assertEquals(size, None)
        assertEquals(releases, 1)
      }
    }
  }

  test("cancellation preserves source cleanup and leaves size unknown") {
    for {
      observer <- BodySizeObserver.create[IO]
      entered <- Deferred[IO, Unit]
      released <- Deferred[IO, Unit]
      source = (body ++ Stream.exec(entered.complete(()).void) ++ Stream.never[IO])
        .onFinalize(released.complete(()).void)
      fiber <- observer.observe(source).compile.drain.start
      _ <- entered.get
      _ <- fiber.cancel
      _ <- released.get
      outcome <- fiber.join
      size <- observer.freeze
    } yield {
      assert(outcome.isCanceled)
      assertEquals(size, None)
    }
  }

  List(false, true).foreach { partialFirst =>
    test(
      s"replay preserves body effects without replacing the first observation: partial=$partialFirst"
    ) {
      for {
        observer <- BodySizeObserver.create[IO]
        evaluations <- Ref.of[IO, Int](0)
        source = Stream.eval(evaluations.getAndUpdate(_ + 1)).flatMap { evaluation =>
          if (evaluation == 0) body else Stream.emit(9.toByte)
        }
        observed = observer.observe(source)
        first <- (if (partialFirst) observed.take(1) else observed).compile.toVector
        second <- observed.compile.toVector
        size <- observer.freeze
        count <- evaluations.get
      } yield {
        assertEquals(first, if (partialFirst) Vector[Byte](1) else Vector[Byte](1, 2, 3, 4, 5))
        assertEquals(second, Vector[Byte](9))
        assertEquals(size, if (partialFirst) None else Some(5L))
        assertEquals(count, 2)
      }
    }
  }

  test("the first concurrent evaluation owns the observation even when another finishes first") {
    for {
      observer <- BodySizeObserver.create[IO]
      evaluations <- Ref.of[IO, Int](0)
      entered <- Deferred[IO, Unit]
      proceed <- Deferred[IO, Unit]
      source = Stream.eval(evaluations.getAndUpdate(_ + 1)).flatMap { evaluation =>
        if (evaluation == 0)
          Stream.exec(entered.complete(()) >> proceed.get) ++ body
        else Stream.emit(9.toByte)
      }
      observed = observer.observe(source)
      first <- observed.compile.toVector.start
      _ <- entered.get
      second <- observed.compile.toVector
      _ <- proceed.complete(())
      result <- first.joinWithNever
      size <- observer.freeze
    } yield {
      assertEquals(result, Vector[Byte](1, 2, 3, 4, 5))
      assertEquals(second, Vector[Byte](9))
      assertEquals(size, Some(5L))
    }
  }

  test("freezing an active observation prevents a late EOF from changing its result") {
    for {
      observer <- BodySizeObserver.create[IO]
      entered <- Deferred[IO, Unit]
      proceed <- Deferred[IO, Unit]
      observed = observer.observe(body ++ Stream.exec(entered.complete(()) >> proceed.get))
      fiber <- observed.compile.toVector.start
      _ <- entered.get
      size <- observer.freeze
      _ <- proceed.complete(())
      bytes <- fiber.joinWithNever
      frozen <- observer.freeze
      _ <- observed.compile.drain
      afterReplay <- observer.freeze
    } yield {
      assertEquals(bytes, Vector[Byte](1, 2, 3, 4, 5))
      assertEquals(size, None)
      assertEquals(frozen, None)
      assertEquals(afterReplay, None)
    }
  }

  test("a downstream failure before EOF preserves the error and leaves size unknown") {
    val boom = new RuntimeException("consumer failure")
    for {
      observer <- BodySizeObserver.create[IO]
      result <- observer.observe(body).evalMap(_ => IO.raiseError[Byte](boom)).compile.drain.attempt
      size <- observer.freeze
    } yield {
      assertEquals(result, Left(boom))
      assertEquals(size, None)
    }
  }
}
