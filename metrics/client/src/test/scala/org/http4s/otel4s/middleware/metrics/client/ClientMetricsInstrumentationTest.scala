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
import munit.CatsEffectSuite
import org.http4s.Header
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
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
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ClientMetricsInstrumentationTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items")
    .putHeaders(Header.Raw(ci"Content-Length", "3"))
  private val start = Attributes(
    HttpAttributes.HttpRequestMethod("GET"),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  List(
    ("all disabled, active absent", false, false, false, false, false),
    ("all disabled, active configured", false, true, false, false, false),
    ("duration only", true, false, false, false, false),
    ("active requests only", false, true, true, false, false),
    ("request size only", false, false, false, true, false),
    ("response size only", false, false, false, false, true),
    ("all enabled", true, true, true, true, true),
  ).foreach {
    case (
          name,
          durationEnabled,
          activeOptedIn,
          activeEnabled,
          requestSizeEnabled,
          responseSizeEnabled,
        ) =>
      test(s"instrument availability: $name") {
        val enabled =
          durationEnabled || activeEnabled || requestSizeEnabled || responseSizeEnabled
        OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
          var filterCalls = 0
          var labelCalls = 0
          val baseConfig =
            if (activeOptedIn) ClientMetricsConfig.default.optIntoActiveRequests
            else ClientMetricsConfig.default
          val config = baseConfig
            .optIntoRequestBodySize(BodySizeConfig.observe)
            .optIntoResponseBodySize
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
          val original = Response[IO]().putHeaders(Header.Raw(ci"Content-Length", "5"))
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
            requestSize <-
              if (requestSizeEnabled)
                meter.histogram[Long](HttpExperimentalMetrics.ClientRequestBodySize.name).create
              else IO.pure(Histogram.noop[IO, Long])
            responseSize <-
              if (responseSizeEnabled)
                meter.histogram[Long](HttpExperimentalMetrics.ClientResponseBodySize.name).create
              else IO.pure(Histogram.noop[IO, Long])
            recorder = new ClientMetricsRecorder[IO](
              testkit,
              config,
              duration,
              active,
              Some(requestSize),
              Some(responseSize),
            )
            middleware = ClientMetricsMiddleware.wrap(recorder, config)
            client = middleware(
              Client[IO](req =>
                Resource.make(req.body.compile.drain.as(original))(_ => releases.update(_ + 1))
              )
            )
            resource = client.run(request)
            _ = assertEquals(filterCalls, 0)
            _ <- resource.use { response =>
              if (!enabled) assert(response eq original)
              response.body.compile.drain
            }
            released <- releases.get
            metrics <- testkit.collectMetrics
          } yield {
            assertEquals(filterCalls, if (enabled) 1 else 0)
            assertEquals(labelCalls, if (enabled) 1 else 0)
            assertEquals(released, 1)
            assertEquals(
              metrics.map(_.name).toSet,
              Option.when(durationEnabled)(HttpMetrics.ClientRequestDuration.name).toSet ++
                Option
                  .when(activeEnabled)(HttpExperimentalMetrics.ClientActiveRequests.name)
                  .toSet ++
                Option
                  .when(requestSizeEnabled)(HttpExperimentalMetrics.ClientRequestBodySize.name)
                  .toSet ++
                Option
                  .when(responseSizeEnabled)(HttpExperimentalMetrics.ClientResponseBodySize.name)
                  .toSet,
            )
          }
        }
      }
  }

  List(
    HttpMetrics.ClientRequestDuration.name,
    HttpExperimentalMetrics.ClientRequestBodySize.name,
    HttpExperimentalMetrics.ClientResponseBodySize.name,
  ).foreach { failedInstrument =>
    test(
      s"failed $failedInstrument recording still attempts other metrics and releases the response"
    ) {
      val boom = new IllegalStateException("recording failure")
      OpenTelemetrySdkTestkit
        .inMemory[IO](
          _.addMeterProviderCustomizer(
            _.registerView(
              InstrumentSelector.builder.withInstrumentName(failedInstrument).build,
              View.builder.addAttributeFilter((_: Attribute[_]) => throw boom).build,
            )
          )
        )
        .use { testkit =>
          val config =
            ClientMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize
          val response = Response[IO]().putHeaders(Header.Raw(ci"Content-Length", "5"))
          for {
            releases <- Ref.of[IO, Int](0)
            middleware <- ClientMetrics.middleware[IO](testkit, config)
            client = middleware(
              Client[IO](_ => Resource.make(IO.pure(response))(_ => releases.update(_ + 1)))
            )
            result <- client.run(request).use(_.body.compile.drain).attempt
            released <- releases.get
            metrics <- testkit.collectMetrics
          } yield {
            assertEquals(result, Left(boom))
            assertEquals(released, 1)
            val terminal = start ++ Attributes(
              HttpAttributes.HttpResponseStatusCode(200L),
              NetworkAttributes.NetworkProtocolVersion("1.1"),
            )
            def size(name: String, bytes: Double): MetricExpectation =
              MetricExpectation
                .histogram(name)
                .exactlyPoints(
                  PointExpectation.histogram.count(1L).sum(bytes).attributesExact(terminal)
                )
            val histograms = List(
              HttpMetrics.ClientRequestDuration.name -> durationMetric(terminal),
              HttpExperimentalMetrics.ClientRequestBodySize.name ->
                size(HttpExperimentalMetrics.ClientRequestBodySize.name, 3d),
              HttpExperimentalMetrics.ClientResponseBodySize.name ->
                size(HttpExperimentalMetrics.ClientResponseBodySize.name, 5d),
            ).collect { case (name, expectation) if name != failedInstrument => expectation }
            assertMetrics(metrics, (activeMetric(0L, start) :: histograms): _*)
          }
        }
    }
  }

  List(false, true).foreach { configured =>
    test(
      s"body-size metrics preserve messages without an enabled observer: configured=$configured"
    ) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        val config =
          if (configured)
            ClientMetricsConfig.default
              .optIntoRequestBodySize(BodySizeConfig.observe)
              .optIntoResponseBodySize(BodySizeConfig.observe)
          else ClientMetricsConfig.default
        val histogram = Option.when(configured)(Histogram.noop[IO, Long])
        val recorder = new ClientMetricsRecorder[IO](
          testkit,
          config,
          Histogram.noop[IO, Double],
          None,
          histogram,
          histogram,
        )
        val original = Response[IO]()
        val prelude = request.requestPrelude
        for {
          exchange <- recorder.start(request, prelude)
          _ = assert(exchange.observeRequest(request) eq request)
          observed <- exchange.observeResponse(prelude, original)
        } yield assert(observed eq original)
      }
    }
  }
}
