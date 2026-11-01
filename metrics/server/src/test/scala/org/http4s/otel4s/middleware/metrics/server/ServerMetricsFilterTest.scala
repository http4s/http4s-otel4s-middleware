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
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes

class ServerMetricsFilterTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val terminal = start ++ Attributes(
    NetworkAttributes.NetworkProtocolVersion("1.1"),
    HttpAttributes.HttpResponseStatusCode(200L),
  )

  List("success", "app-error", "app-cancel", "body-error", "body-cancel").foreach { mode =>
    test(s"excluded invocations preserve HTTP behavior and skip callbacks: $mode") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        val boom = new IllegalStateException("HTTP failure")
        var filters = 0
        val config = ServerMetricsConfig.default.optIntoActiveRequests
          .withRequestFilter { prelude =>
            filters += 1
            assertEquals(prelude.uri, request.uri)
            PerRequestConfig.Disabled
          }
          .withRequestAttributes(_ => fail("excluded request labels"))
          .withRouteClassifier(_ => fail("excluded route classifier"))
          .withErrorClassifier((_, _) => fail("excluded error classifier"))
        for {
          entered <- Deferred[IO, Unit]
          body =
            if (mode == "body-error") Stream.raiseError[IO](boom)
            else if (mode == "body-cancel")
              Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
            else Stream.empty
          original = Response[IO]().withBodyStream(body)
          middleware <- ServerMetrics.middleware[IO](kit, config)
          app = middleware(HttpApp[IO] { req =>
            assert(req eq request)
            if (mode == "app-error") IO.raiseError[Response[IO]](boom)
            else if (mode == "app-cancel") entered.complete(()).void >> IO.never[Response[IO]]
            else IO.pure(original)
          })
          effect = app(request).flatMap { response =>
            assert(response eq original)
            response.body.compile.drain
          }
          _ = assertEquals(filters, 0)
          _ <-
            if (mode.endsWith("cancel")) for {
              fiber <- effect.start
              _ <- entered.get
              _ <- fiber.cancel
              outcome <- fiber.join
              _ = assert(outcome.isCanceled)
            } yield ()
            else
              effect.attempt.flatMap(result =>
                IO(assertEquals(result, if (mode.endsWith("error")) Left(boom) else Right(())))
              )
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(filters, 1)
          assertNoMetrics(metrics)
        }
      }
    }
  }

  test("request prelude selects which invocations are measured") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val config = ServerMetricsConfig.default.optIntoActiveRequests.withRequestFilter { request =>
        if (request.method == Method.GET) PerRequestConfig.Enabled else PerRequestConfig.Disabled
      }
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config)
        app = middleware(HttpApp[IO](_ => IO.pure(Response[IO]())))
        _ <- app(request).flatMap(_.body.compile.drain)
        _ <- app(request.withMethod(Method.POST)).flatMap(_.body.compile.drain)
        metrics <- kit.collectMetrics
      } yield assertMetrics(metrics, activeMetric(0L, start), durationMetric(terminal))
    }
  }
}
