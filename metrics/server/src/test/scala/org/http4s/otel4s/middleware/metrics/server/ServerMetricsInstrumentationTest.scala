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

import cats.effect.IO
import cats.effect.Ref
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.Histogram
import org.typelevel.otel4s.metrics.UpDownCounter
import org.typelevel.otel4s.sdk.metrics.view.InstrumentSelector
import org.typelevel.otel4s.sdk.metrics.view.View
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ServerMetricsInstrumentationTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val terminal = start ++ Attributes(
    NetworkAttributes.NetworkProtocolVersion("1.1"),
    HttpAttributes.HttpResponseStatusCode(200L),
  )

  List(
    ("all disabled, active absent", false, false, false),
    ("all disabled, active configured", false, true, false),
    ("duration only", true, false, false),
    ("active requests only", false, true, true),
    ("all enabled", true, true, true),
  ).foreach { case (name, durationEnabled, activeOptedIn, activeEnabled) =>
    test(s"instrument availability: $name") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        val enabled = durationEnabled || activeEnabled
        var filters = 0
        var labels = 0
        val baseConfig =
          if (activeOptedIn) ServerMetricsConfig.default.optIntoActiveRequests
          else ServerMetricsConfig.default
        val config = baseConfig
          .withRequestFilter { _ =>
            filters += 1
            if (!enabled) fail("disabled request filter")
            PerRequestConfig.Enabled
          }
          .withRequestAttributes { _ => labels += 1; Attributes.empty }
          .withRouteClassifier { _ => if (!enabled) fail("disabled route classifier"); None }
          .withErrorClassifier { (_, _) => if (!enabled) fail("disabled error classifier"); false }
        for {
          cleanups <- Ref.of[IO, Int](0)
          original = Response[IO]().withBodyStream(Stream.exec(cleanups.update(_ + 1)))
          meter <- kit.meterProvider.get("test")
          duration <-
            if (durationEnabled)
              meter.histogram[Double](HttpMetrics.ServerRequestDuration.name).create
            else IO.pure(Histogram.noop[IO, Double])
          active <-
            if (!activeOptedIn) IO.pure(Option.empty[UpDownCounter[IO, Long]])
            else if (activeEnabled)
              meter
                .upDownCounter[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                .create
                .map(Option(_))
            else IO.pure(Some(UpDownCounter.noop[IO, Long]): Option[UpDownCounter[IO, Long]])
          recorder = new ServerMetricsRecorder[IO](kit, config, duration, active)
          middleware = ServerMetricsMiddleware.wrap(recorder, config)
          app = middleware(HttpApp[IO] { req =>
            assert(req eq request)
            IO.pure(original)
          })
          effect = app(request)
          _ = assertEquals(filters, 0)
          _ <- effect.flatMap { response =>
            if (!enabled) assert(response eq original)
            response.body.compile.drain
          }
          _ <- effect.flatMap(_.body.compile.drain)
          count <- cleanups.get
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(filters, if (enabled) 2 else 0)
          assertEquals(labels, if (enabled) 2 else 0)
          assertEquals(count, 2)
          if (durationEnabled)
            assertMetrics(
              metrics,
              MetricExpectation
                .histogram(HttpMetrics.ServerRequestDuration.name)
                .exactlyPoints(
                  PointExpectation.histogram.count(2L).attributesExact(terminal)
                ),
            )
          if (activeEnabled) assertMetrics(metrics, activeMetric(0L, start))
          assertEquals(
            metrics.map(_.name).toSet,
            Option.when(durationEnabled)(HttpMetrics.ServerRequestDuration.name).toSet ++
              Option.when(activeEnabled)(HttpExperimentalMetrics.ServerActiveRequests.name).toSet,
          )
        }
      }
    }
  }

  List("duration recording", "error classification").foreach { failure =>
    test(s"failed $failure still balances active requests and preserves body cleanup") {
      val boom = new IllegalStateException("recording failure")
      OpenTelemetrySdkTestkit
        .inMemory[IO](
          _.addMeterProviderCustomizer(
            _.registerView(
              InstrumentSelector.builder
                .withInstrumentName(HttpMetrics.ServerRequestDuration.name)
                .build,
              View.builder
                .addAttributeFilter((_: Attribute[_]) =>
                  if (failure == "duration recording") throw boom else true
                )
                .build,
            )
          )
        )
        .use { kit =>
          for {
            cleanups <- Ref.of[IO, Int](0)
            config = ServerMetricsConfig.default.optIntoActiveRequests.withErrorClassifier { (_, _) =>
              if (failure == "error classification") throw boom else false
            }
            middleware <- ServerMetrics.middleware[IO](kit, config)
            response <- middleware(
              HttpApp[IO](_ =>
                IO.pure(
                  Response[IO]().withBodyStream(Stream.empty.onFinalize(cleanups.update(_ + 1)))
                )
              )
            )(request)
            result <- response.body.compile.drain.attempt
            count <- cleanups.get
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(result, Left(boom))
            assertEquals(count, 1)
            assertMetrics(metrics, activeMetric(0L, start))
          }
        }
    }
  }
}
