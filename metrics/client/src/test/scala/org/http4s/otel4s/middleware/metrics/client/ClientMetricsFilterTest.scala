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

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.functor._
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ClientMetricsFilterTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val startAttributes = Attributes(
    HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  List("success", "acquire-error", "body-error", "body-cancel").foreach { mode =>
    test(s"filtering: excluded requests preserve HTTP behavior and record no metrics: $mode") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        for {
          middleware <- ClientMetrics
            .middleware[IO](
              testkit,
              ClientMetricsConfig.default.optIntoActiveRequests
                .withRequestFilter(PerRequestFilter.neverEnabled),
            )
          entered <- Deferred[IO, Unit]
          snapshots <- Ref.of[IO, Vector[List[MetricData]]](Vector.empty)
          releases <- Ref.of[IO, Int](0)
          boom = new RuntimeException("HTTP failure")
          snapshot = testkit.collectMetrics.flatMap(m => snapshots.update(_ :+ m))
          body = Stream.exec(snapshot) ++ Stream.emits(List[Byte](1, 2, 3)).covary[IO] ++
            (mode match {
              case "body-error" => Stream.raiseError[IO](boom)
              case "body-cancel" => Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
              case _ => Stream.empty
            })
          original = Response[IO]().withBodyStream(body)
          acquire = snapshot >> (if (mode == "acquire-error") IO.raiseError[Response[IO]](boom)
                                 else IO.pure(original))
          base = Client[IO](_ => Resource.make(acquire)(_ => releases.update(_ + 1) >> snapshot))
          use = middleware(base).run(request).use { response =>
            assert(response eq original)
            response.body.compile.toVector
          }
          _ <-
            if (mode == "body-cancel") for {
              fiber <- use.start
              _ <- entered.get
              _ <- fiber.cancel
            } yield ()
            else
              use.attempt.flatMap { result =>
                IO(
                  assertEquals(
                    result,
                    if (mode.endsWith("error")) Left(boom) else Right(Vector[Byte](1, 2, 3)),
                  )
                )
              }
          released <- releases.get
          during <- snapshots.get
          metrics <- testkit.collectMetrics
        } yield {
          assertEquals(released, if (mode == "acquire-error") 0 else 1)
          (during :+ metrics).foreach(assertNoMetrics)
        }
      }
    }
  }

  test("filtering: request prelude selects which exchanges are measured") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      for {
        middleware <- ClientMetrics
          .middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests.withRequestFilter(req =>
              if (req.uri.path != uri"/health".path) PerRequestConfig.Enabled
              else PerRequestConfig.Disabled
            ),
          )
        calls <- Ref.of[IO, Int](0)
        base = Client[IO](_ => Resource.eval(calls.update(_ + 1)).as(Response[IO]()))
        client = middleware(base)
        _ <- client.run(request.withUri(uri"https://example.test/health")).use(_.body.compile.drain)
        excluded <- testkit.collectMetrics
        _ = assertNoMetrics(excluded)
        _ <- client.run(request).use(_.body.compile.drain)
        _ <- client.run(request.withUri(uri"https://example.test/health")).use(_ => IO.unit)
        count <- calls.get
        metrics <- testkit.collectMetrics
      } yield {
        assertEquals(count, 3)
        assertOneExchange(metrics)
      }
    }
  }

  test(
    "filtering: the predicate is lazy and evaluated once for each acquisition of a reused resource"
  ) {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      var evaluations = 0
      for {
        middleware <- ClientMetrics.middleware[IO](
          testkit,
          ClientMetricsConfig.default.optIntoActiveRequests.withRequestFilter { _ =>
            evaluations += 1
            if (evaluations == 2) PerRequestConfig.Enabled else PerRequestConfig.Disabled
          },
        )
        base = Client[IO](_ => Resource.pure(Response[IO]()))
        response = middleware(base).run(request)
        _ = assertEquals(evaluations, 0)
        _ <- response.use(_.body.compile.drain)
        _ = assertEquals(evaluations, 1)
        excluded <- testkit.collectMetrics
        _ = assertNoMetrics(excluded)
        _ <- response.use(_.body.compile.drain)
        _ <- response.use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield {
        assertEquals(evaluations, 3)
        assertOneExchange(metrics)
      }
    }
  }

  test("filtering: excluded requests skip attribute, template and error callbacks") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      for {
        middleware <- ClientMetrics.middleware[IO](
          testkit,
          ClientMetricsConfig.default.optIntoActiveRequests
            .withRequestFilter(PerRequestFilter.neverEnabled)
            .withRequestAttributes(_ => fail("request callback"))
            .withUrlTemplateClassifier(_ => fail("template callback"))
            .withErrorClassifier((_, _) => fail("error callback")),
        )
        client = middleware(Client[IO](_ => Resource.pure(Response[IO](Status.ServiceUnavailable))))
        _ <- client.run(request).use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield assertNoMetrics(metrics)
    }
  }

  private def assertOneExchange(metrics: List[MetricData]): Unit =
    MetricExpectations
      .checkAllDistinct(
        metrics,
        MetricExpectation
          .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
          .exactlyPoints(PointExpectation.numeric(0L).attributesExact(startAttributes)),
        MetricExpectation
          .histogram(HttpMetrics.ClientRequestDuration.name)
          .exactlyPoints(
            PointExpectation.histogram
              .count(1L)
              .attributesExact(
                startAttributes ++ Attributes(
                  HttpAttributes.HttpResponseStatusCode(200L),
                  NetworkAttributes.NetworkProtocolVersion("1.1"),
                )
              )
          ),
      )
      .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
}
