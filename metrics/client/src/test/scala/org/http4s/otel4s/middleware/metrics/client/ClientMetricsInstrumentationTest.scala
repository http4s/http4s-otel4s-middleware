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

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all._
import munit.CatsEffectSuite
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.Histogram
import org.typelevel.otel4s.metrics.UpDownCounter
import org.typelevel.otel4s.sdk.metrics.view.InstrumentSelector
import org.typelevel.otel4s.sdk.metrics.view.View
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ClientMetricsInstrumentationTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val start = Attributes(
    HttpAttributes.HttpRequestMethod("GET"),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  List(
    (false, false, false),
    (false, true, false),
    (false, true, true),
    (true, true, false),
    (true, true, true),
  ).foreach { case (durationEnabled, activeOptedIn, activeEnabled) =>
    test(
      s"instrument availability: duration=$durationEnabled, opt-in=$activeOptedIn, active=$activeEnabled"
    ) {
      val enabled = durationEnabled || activeEnabled
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        var filterCalls = 0
        var labelCalls = 0
        val baseConfig =
          if (activeOptedIn) ClientMetricsConfig.default.optIntoActiveRequests
          else ClientMetricsConfig.default
        val config = baseConfig
          .withRequestFilter { _ =>
            filterCalls += 1
            if (!enabled) fail("disabled instruments evaluated the request filter")
            PerRequestConfig.Enabled
          }
          .withRequestAttributes { _ => labelCalls += 1; Attributes.empty }
          .withUrlTemplateClassifier { _ =>
            if (!enabled) fail("disabled instruments evaluated the template classifier")
            None
          }
          .withErrorClassifier { (_, _) =>
            if (!enabled) fail("disabled instruments evaluated the response classifier")
            false
          }
        val original = Response[IO]()
        for {
          releases <- Ref.of[IO, Int](0)
          meter <- testkit.meterProvider.get("test")
          duration <-
            if (durationEnabled)
              meter.histogram[Double](HttpMetrics.ClientRequestDuration.name).create
            else IO.pure(Histogram.noop[IO, Double])
          active <-
            if (!activeOptedIn) IO.pure(Option.empty[UpDownCounter[IO, Long]])
            else if (activeEnabled)
              meter
                .upDownCounter[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
                .create
                .map(Option(_))
            else IO.pure(Some(UpDownCounter.noop[IO, Long]): Option[UpDownCounter[IO, Long]])
          recorder = new ClientMetricsRecorder[IO](testkit, config, duration, active)
          middleware = ClientMetricsMiddleware.wrap(recorder, config)
          client = middleware(
            Client[IO](_ => Resource.make(IO.pure(original))(_ => releases.update(_ + 1)))
          )
          resource = client.run(request)
          _ = assertEquals(filterCalls, 0)
          _ <- List
            .fill(2)(resource.use { response =>
              if (!enabled) assert(response eq original)
              response.body.compile.drain
            })
            .sequence_
          released <- releases.get
          metrics <- testkit.collectMetrics
        } yield {
          assertEquals(filterCalls, if (enabled) 2 else 0)
          assertEquals(labelCalls, if (enabled) 2 else 0)
          assertEquals(released, 2)
          assertEquals(
            metrics.map(_.name).toSet,
            Option.when(durationEnabled)(HttpMetrics.ClientRequestDuration.name).toSet ++
              Option
                .when(activeEnabled)(HttpExperimentalMetrics.ClientActiveRequests.name)
                .toSet,
          )
        }
      }
    }
  }

  test("failed duration recording still balances active requests and releases the response") {
    val boom = new IllegalStateException("duration recording")
    OpenTelemetrySdkTestkit
      .inMemory[IO](
        _.addMeterProviderCustomizer(
          _.registerView(
            InstrumentSelector.builder
              .withInstrumentName(HttpMetrics.ClientRequestDuration.name)
              .build,
            View.builder
              .addAttributeFilter((_: Attribute[_]) => throw boom)
              .build,
          )
        )
      )
      .use { testkit =>
        for {
          releases <- Ref.of[IO, Int](0)
          middleware <- ClientMetrics
            .middleware[IO](testkit, ClientMetricsConfig.default.optIntoActiveRequests)
          client = middleware(
            Client[IO](_ => Resource.make(IO.pure(Response[IO]()))(_ => releases.update(_ + 1)))
          )
          result <- client.run(request).use(_.body.compile.drain).attempt
          released <- releases.get
          metrics <- testkit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          assertEquals(released, 1)
          assertMetrics(metrics, activeMetric(0L, start))
        }
      }
  }
}
