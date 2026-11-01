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

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

import scala.concurrent.duration._

class ServerMetricsLifecycleTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items/123")
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val protocol = Attributes(NetworkAttributes.NetworkProtocolVersion("1.1"))

  List(
    "success",
    "empty",
    "partial",
    "unread",
    "consumer-error",
    "post-body-error",
    "body-error",
    "caught-body-error",
    "app-error",
    "app-cancel",
    "body-cancel",
    "replay",
    "error-status",
    "cleanup-error",
    "downstream-error",
  ).foreach { mode =>
    test(s"duration and active requests follow the selected server boundary: $mode") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val boom = new IllegalStateException("source or consumer failure")
          val status = if (mode == "error-status") Status.ServiceUnavailable else Status.Ok
          for {
            entered <- Deferred[IO, Unit]
            during <- Ref.of[IO, List[MetricData]](Nil)
            afterApp <- Ref.of[IO, List[MetricData]](Nil)
            cleanups <- Ref.of[IO, Int](0)
            middleware <- ServerMetrics
              .middleware[IO](kit, ServerMetricsConfig.default.optIntoActiveRequests)
            cleanup = IO.sleep(5.seconds) >> cleanups.update(_ + 1) >>
              (if (mode == "cleanup-error") IO.raiseError[Unit](boom) else IO.unit)
            body = (Stream.sleep_[IO](3.seconds) ++
              (if (mode == "body-cancel") Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
               else if (mode == "empty") Stream.empty
               else
                 Stream.emits(List[Byte](1, 2, 3)).covary[IO] ++
                   (if (Set("body-error", "caught-body-error")(mode)) Stream.raiseError[IO](boom)
                    else Stream.empty))).onFinalize(cleanup)
            app = middleware(
              HttpApp[IO](_ =>
                IO.sleep(2.seconds) >> kit.collectMetrics.flatMap(during.set) >>
                  (if (mode == "app-error") IO.raiseError[Response[IO]](boom)
                   else if (mode == "app-cancel")
                     entered.complete(()).void >> IO.never[Response[IO]]
                   else IO.pure(Response[IO](status).withBodyStream(body)))
              )
            )
            run = app(request).flatMap { response =>
              kit.collectMetrics.flatMap(afterApp.set) >> (mode match {
                case "unread" => IO.sleep(7.seconds)
                case "consumer-error" => IO.sleep(7.seconds) >> IO.raiseError[Unit](boom)
                case "post-body-error" =>
                  response.body.compile.drain >> IO.sleep(7.seconds) >> IO.raiseError[Unit](boom)
                case "partial" =>
                  response.body
                    .take(1)
                    .compile
                    .toVector
                    .flatMap(bytes => IO(assertEquals(bytes, Vector[Byte](1))))
                case "caught-body-error" =>
                  response.body.handleErrorWith(_ => Stream.empty).compile.drain
                case "downstream-error" =>
                  response.body.evalMap(_ => IO.raiseError[Byte](boom)).compile.drain
                case "replay" => response.body.compile.drain >> response.body.compile.drain
                case _ => response.body.compile.drain
              })
            }
            _ <-
              if (mode.endsWith("-cancel")) for {
                fiber <- run.start
                _ <- entered.get
                _ <- fiber.cancel
              } yield ()
              else
                run.attempt.flatMap { result =>
                  val failed = Set(
                    "consumer-error",
                    "post-body-error",
                    "body-error",
                    "app-error",
                    "cleanup-error",
                    "downstream-error",
                  )(mode)
                  IO(assertEquals(result, if (failed) Left(boom) else Right(())))
                }
            inFlight <- during.get
            returned <- afterApp.get
            metrics <- kit.collectMetrics
            finalized <- cleanups.get
            spans <- kit.finishedSpans
          } yield {
            assertMetrics(inFlight, activeMetric(1L, start))
            if (!mode.startsWith("app-")) assertMetrics(returned, activeMetric(1L, start))
            val unstarted = Set("unread", "consumer-error")(mode)
            if (unstarted) {
              assertMetrics(metrics, activeMetric(1L, start))
              assertEquals(
                MetricExpectations
                  .find(metrics, MetricExpectation.name(HttpMetrics.ServerRequestDuration.name)),
                None,
              )
            } else {
              val response =
                if (mode.startsWith("app-")) Attributes.empty
                else Attributes(HttpAttributes.HttpResponseStatusCode(status.code.toLong))
              val error =
                if (Set("app-error", "body-error", "caught-body-error", "cleanup-error")(mode))
                  Some(ErrorAttributes.ErrorType(boom.getClass.getName))
                else if (mode == "error-status") Some(ErrorAttributes.ErrorType("503"))
                else None
              assertMetrics(
                metrics,
                activeMetric(0L, start),
                durationMetric(
                  if (mode.startsWith("app-")) 2d else 10d,
                  start ++ protocol ++ response ++ error,
                ),
              )
            }
            assertEquals(
              finalized,
              if (unstarted || mode.startsWith("app-")) 0 else if (mode == "replay") 2 else 1,
            )
            TraceExpectations
              .check(spans, TraceForestExpectation.empty)
              .fold(m => fail(TraceExpectations.format(m)), identity)
          }
        }
      }
    }
  }

  List((Method.HEAD, Status.Ok), (Method.GET, Status.NoContent), (Method.GET, Status.NotModified))
    .foreach { case (method, status) =>
      test(s"an unstarted empty body stays incomplete even for $method/${status.code}") {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          for {
            middleware <- ServerMetrics
              .middleware[IO](kit, ServerMetricsConfig.default.optIntoActiveRequests)
            _ <- middleware(HttpApp[IO](_ => IO.pure(Response[IO](status))))(
              request.withMethod(method)
            )
            metrics <- kit.collectMetrics
          } yield {
            assertMetrics(
              metrics,
              activeMetric(1L, start.added(HttpAttributes.HttpRequestMethod(method.name))),
            )
            assertEquals(
              MetricExpectations
                .find(metrics, MetricExpectation.name(HttpMetrics.ServerRequestDuration.name)),
              None,
            )
          }
        }
      }
    }

  test("body wrapping preserves demand, chunks, request effects, and cleanup") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        pulls <- Ref.of[IO, Int](0)
        requestEffects <- Ref.of[IO, Int](0)
        releases <- Ref.of[IO, Int](0)
        middleware <- ServerMetrics
          .middleware[IO](kit, ServerMetricsConfig.default.optIntoActiveRequests)
        first = fs2.Chunk.array(Array[Byte](1, 2, 3))
        second = fs2.Chunk.array(Array[Byte](4, 5))
        body = (Stream.eval(pulls.update(_ + 1)).drain ++ Stream.chunk(first) ++
          Stream.eval(pulls.update(_ + 1)).drain ++ Stream.chunk(second))
          .onFinalize(releases.update(_ + 1))
        response <- middleware(
          HttpApp[IO](req => req.body.compile.drain.as(Response[IO]().withBodyStream(body)))
        )(request.withBodyStream(Stream.exec(requestEffects.update(_ + 1))))
        before <- pulls.get
        chunks <- response.body.chunks.compile.toVector
        after <- pulls.get
        effects <- requestEffects.get
        closed <- releases.get
      } yield {
        assertEquals(before, 0)
        assertEquals(chunks, Vector(first, second))
        assertEquals(after, 2)
        assertEquals(effects, 1)
        assertEquals(closed, 1)
      }
    }
  }

  test("concurrent body evaluations can complete the exchange only once") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        entered <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        evaluations <- Ref.of[IO, Int](0)
        middleware <- ServerMetrics
          .middleware[IO](kit, ServerMetricsConfig.default.optIntoActiveRequests)
        body = Stream.exec(evaluations.updateAndGet(_ + 1).flatMap {
          case 2 => entered.complete(()).void
          case _ => IO.unit
        } >> release.get)
        response <- middleware(HttpApp[IO](_ => IO.pure(Response[IO]().withBodyStream(body))))(
          request
        )
        first <- response.body.compile.drain.start
        second <- response.body.compile.drain.start
        _ <- entered.get >> release.complete(())
        _ <- first.joinWithNever >> second.joinWithNever
        metrics <- kit.collectMetrics
      } yield assertMetrics(
        metrics,
        activeMetric(0L, start),
        MetricExpectation
          .histogram(HttpMetrics.ServerRequestDuration.name)
          .exactlyPoints(
            PointExpectation.histogram
              .count(1L)
              .attributesExact(
                start ++ protocol ++
                  Attributes(HttpAttributes.HttpResponseStatusCode(200L))
              )
          ),
      )
    }
  }
}
