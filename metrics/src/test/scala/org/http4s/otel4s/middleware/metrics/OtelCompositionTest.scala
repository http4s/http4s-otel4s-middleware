/*
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

import cats.~>
import cats.effect.IO
import cats.syntax.all._
import fs2.Chunk
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.server.middleware.Metrics
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.testkit.metrics.MetricsTestkit
import org.typelevel.otel4s.sdk.testkit.trace.TracesTestkit
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.TracerProvider

class OtelCompositionTest extends CatsEffectSuite {
  private object NoopRedactor extends PathRedactor.NeverRedact with QueryRedactor.NeverRedact

  List("full", "partial", "cancel", "error", "cleanup-error").foreach { mode =>
    test(s"tracing outside metrics retains an open span in every callback: $mode") {
      (TracesTestkit.inMemory[IO](), MetricsTestkit.inMemory[IO]()).tupled.use {
        case (traces, metrics) =>
          implicit val tp: TracerProvider[IO] = traces.tracerProvider
          implicit val mp: MeterProvider[IO] = metrics.meterProvider
          for {
            tracer <- tp.get("probe")
            middleware <- ServerMiddleware
              .builder[IO](ServerSpanDataProvider.openTelemetry(NoopRedactor))
              .build
            observations <- IO.ref(List.empty[(Option[SpanContext], Int)])
            ops <- OtelMetrics.serverMetricsOps[IO](ServerMetricsConfig.all)
            instrumented = ops.mapK(new (IO ~> IO) {
              def apply[A](fa: IO[A]): IO[A] = for {
                context <- tracer.currentSpanContext
                finished <- traces.finishedSpans
                _ <- observations.update(_ :+ (context, finished.size))
                a <- fa
              } yield a
            })
            started <- IO.deferred[Unit]
            bytes = Stream.chunk(Chunk.array(Array[Byte](1, 2, 3))).covary[IO]
            body = mode match {
              case "cancel" => Stream.eval(started.complete(())).drain ++ Stream.never[IO]
              case "error" => bytes ++ Stream.raiseError[IO](new IllegalStateException("body"))
              case "cleanup-error" =>
                bytes.onFinalizeWeak(IO.raiseError[Unit](new IllegalStateException("cleanup")))
              case _ => bytes
            }
            app = middleware.wrapHttpApp(
              Metrics[IO](instrumented)(HttpApp.pure[IO](Response[IO]().withBodyStream(body)))
            )
            response <- app.run(Request[IO]())
            _ <-
              if (mode == "cancel") for {
                fiber <- response.body.compile.drain.start
                _ <- started.get
                _ <- fiber.cancel
              } yield ()
              else if (mode == "partial") response.body.take(1).compile.drain
              else response.body.compile.drain.attempt.void
            seen <- observations.get
            spans <- traces.finishedSpans
            restored <- tracer.currentSpanContext
          } yield {
            assertEquals(spans.size, 1)
            assertEquals(seen.size, 7)
            assert(seen.forall(_ == (Some(spans.head.spanContext), 0)), seen.toString)
            assertEquals(restored, None)
          }
      }
    }
  }
}
*/
