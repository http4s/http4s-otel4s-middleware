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
import org.http4s.Header
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
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
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ServerMetricsInstrumentationTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items")
    .putHeaders(Header.Raw(ci"Content-Length", "3"))
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val terminal = start ++ Attributes(
    NetworkAttributes.NetworkProtocolVersion("1.1"),
    HttpAttributes.HttpResponseStatusCode(200L),
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
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val enabled =
            durationEnabled || activeEnabled || requestSizeEnabled || responseSizeEnabled
          var filters = 0
          var labels = 0
          val baseConfig =
            if (activeOptedIn) ServerMetricsConfig.default.optIntoActiveRequests
            else ServerMetricsConfig.default
          val config = baseConfig.optIntoRequestBodySize.optIntoResponseBodySize
            .withRequestFilter { _ =>
              filters += 1
              if (!enabled) fail("disabled request filter")
              PerRequestConfig.Enabled
            }
            .withRequestAttributes { _ => labels += 1; Attributes.empty }
            .withRouteClassifier { _ => if (!enabled) fail("disabled route classifier"); None }
            .withErrorClassifier { (_, _) =>
              if (!enabled) fail("disabled error classifier"); false
            }
          for {
            cleanups <- Ref.of[IO, Int](0)
            original = Response[IO]()
              .withBodyStream(Stream.exec(cleanups.update(_ + 1)))
              .putHeaders(Header.Raw(ci"Content-Length", "5"))
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
            requestSize <-
              if (requestSizeEnabled)
                meter.histogram[Long](HttpExperimentalMetrics.ServerRequestBodySize.name).create
              else IO.pure(Histogram.noop[IO, Long])
            responseSize <-
              if (responseSizeEnabled)
                meter.histogram[Long](HttpExperimentalMetrics.ServerResponseBodySize.name).create
              else IO.pure(Histogram.noop[IO, Long])
            recorder = new ServerMetricsRecorder[IO](
              kit,
              config,
              duration,
              active,
              Some(requestSize),
              Some(responseSize),
            )
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
            if (requestSizeEnabled)
              assertMetrics(
                metrics,
                sizeMetric(HttpExperimentalMetrics.ServerRequestBodySize.name, 6d, 2L),
              )
            if (responseSizeEnabled)
              assertMetrics(
                metrics,
                sizeMetric(HttpExperimentalMetrics.ServerResponseBodySize.name, 10d, 2L),
              )
            assertEquals(
              metrics.map(_.name).toSet,
              Option.when(durationEnabled)(HttpMetrics.ServerRequestDuration.name).toSet ++
                Option
                  .when(activeEnabled)(HttpExperimentalMetrics.ServerActiveRequests.name)
                  .toSet ++
                Option
                  .when(requestSizeEnabled)(HttpExperimentalMetrics.ServerRequestBodySize.name)
                  .toSet ++
                Option
                  .when(responseSizeEnabled)(HttpExperimentalMetrics.ServerResponseBodySize.name)
                  .toSet,
            )
          }
        }
      }
  }

  test("a failed error classifier still balances active requests and preserves body cleanup") {
    val boom = new IllegalStateException("classification failure")
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        cleanups <- Ref.of[IO, Int](0)
        middleware <- ServerMetrics.middleware[IO](
          kit,
          ServerMetricsConfig.default.optIntoActiveRequests
            .withErrorClassifier((_, _) => throw boom),
        )
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

  List(
    HttpMetrics.ServerRequestDuration.name,
    HttpExperimentalMetrics.ServerRequestBodySize.name,
    HttpExperimentalMetrics.ServerResponseBodySize.name,
  ).foreach { failedInstrument =>
    test(s"failed $failedInstrument still attempts other histograms and balances active requests") {
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
        .use { kit =>
          val config =
            ServerMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize
          for {
            cleanups <- Ref.of[IO, Int](0)
            middleware <- ServerMetrics.middleware[IO](kit, config)
            response <- middleware(
              HttpApp[IO](_ =>
                IO.pure(
                  Response[IO]()
                    .putHeaders(Header.Raw(ci"Content-Length", "5"))
                    .withBodyStream(Stream.empty.onFinalize(cleanups.update(_ + 1)))
                )
              )
            )(request)
            result <- response.body.compile.drain.attempt
            metrics <- kit.collectMetrics
            count <- cleanups.get
          } yield {
            assertEquals(result, Left(boom))
            assertEquals(count, 1)
            val histograms = List(
              HttpMetrics.ServerRequestDuration.name -> durationMetric(terminal),
              HttpExperimentalMetrics.ServerRequestBodySize.name -> sizeMetric(
                HttpExperimentalMetrics.ServerRequestBodySize.name,
                3d,
                1L,
              ),
              HttpExperimentalMetrics.ServerResponseBodySize.name -> sizeMetric(
                HttpExperimentalMetrics.ServerResponseBodySize.name,
                5d,
                1L,
              ),
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
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        val config =
          if (configured)
            ServerMetricsConfig.default
              .optIntoRequestBodySize(BodySizeConfig.observe)
              .optIntoResponseBodySize(BodySizeConfig.observe)
          else ServerMetricsConfig.default
        val histogram = Option.when(configured)(Histogram.noop[IO, Long])
        val recorder = new ServerMetricsRecorder[IO](
          kit,
          config,
          Histogram.noop[IO, Double],
          None,
          histogram,
          histogram,
        )
        val response = Response[IO]()
        for {
          recording <- recorder.start(request, request.requestPrelude)
          _ = assert(recording.observeRequest(request) eq request)
        } yield assert(recording.observeResponse(response) eq response.body)
      }
    }
  }

  private def sizeMetric(name: String, value: Double, count: Long): MetricExpectation =
    MetricExpectation
      .histogram(name)
      .exactlyPoints(
        PointExpectation.histogram.count(count).sum(value).attributesExact(terminal)
      )

}
