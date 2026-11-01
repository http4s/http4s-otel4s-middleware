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
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ServerMetricsConfigTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items/123")
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val protocol = Attributes(NetworkAttributes.NetworkProtocolVersion("1.1"))

  test("filter and labels run lazily once per invocation, with immutable labels at decrement") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      var filters = 0
      var labels = 0
      var value = "captured"
      val shared = Attribute("component", "server")
      val operation = Attribute("operation", "captured")
      val settings = ServerMetricsConfig.default.optIntoActiveRequests
        .withAttributes(Attributes(shared))
        .withRequestFilter { _ => filters += 1; PerRequestConfig.Enabled }
        .withRequestAttributes { _ => labels += 1; Attributes(Attribute("operation", value)) }
      for {
        middleware <- ServerMetrics.middleware[IO](kit, settings)
        responseF = middleware(HttpApp[IO](_ => IO.pure(Response[IO]())))(request)
        _ = assertEquals(filters, 0)
        _ = assertEquals(labels, 0)
        response <- responseF
        _ = value = "changed"
        _ <- response.body.compile.drain
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(filters, 1)
        assertEquals(labels, 1)
        val captured = start ++ Attributes(shared, operation)
        assertMetrics(
          metrics,
          activeMetric(0L, captured),
          MetricExpectation
            .histogram(HttpMetrics.ServerRequestDuration.name)
            .exactlyPoints(
              PointExpectation.histogram
                .count(1L)
                .attributesExact(
                  captured ++ protocol ++ Attributes(HttpAttributes.HttpResponseStatusCode(200L))
                )
            ),
        )
      }
    }
  }

  test(
    "configuration is immutable and reusable; repeated invocations have separate completion guards"
  ) {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val defaults = ServerMetricsConfig.default.optIntoActiveRequests
      val buckets = BucketBoundaries(Vector(0d, 1d, 10d))
      val settings = defaults.optIntoServerAddress.optIntoServerPort
        .withResponseDurationSecondsHistogramBuckets(buckets)
      assertEquals(defaults.serverAddressEnabled, false)
      assertEquals(defaults.serverPortEnabled, false)
      assertEquals(settings.serverAddressEnabled, true)
      assertEquals(settings.serverPortEnabled, true)
      for {
        middleware <- ServerMetrics
          .middleware[IO](kit, defaults.withResponseDurationSecondsHistogramBuckets(buckets))
        effect = middleware(HttpApp[IO](_ => IO.pure(Response[IO]())))(request)
        first <- effect
        second <- effect
        _ <- first.body.compile.drain >> second.body.compile.drain >> first.body.compile.drain
        metrics <- kit.collectMetrics
      } yield assertMetrics(
        metrics,
        activeMetric(0L, start),
        MetricExpectation
          .histogram(HttpMetrics.ServerRequestDuration.name)
          .exactlyPoints(
            PointExpectation.histogram
              .count(2L)
              .boundaries(buckets)
              .attributesExact(
                start ++ protocol ++ Attributes(HttpAttributes.HttpResponseStatusCode(200L))
              )
          ),
      )
    }
  }
  List(false, true).foreach { enabled =>
    test(s"active requests: explicit opt-in=$enabled") {
      val original = ServerMetricsConfig.default
      val config = if (enabled) original.optIntoActiveRequests.optIntoActiveRequests else original
      assertEquals(original.activeRequestsEnabled, false)
      assertEquals(config.activeRequestsEnabled, enabled)
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config)
          response <- middleware(HttpApp[IO](_ => IO.pure(Response[IO]())))(request)
          _ <- response.body.compile.drain
          metrics <- kit.collectMetrics
        } yield {
          assertMetrics(
            metrics,
            durationMetric(
              start ++ protocol ++ Attributes(HttpAttributes.HttpResponseStatusCode(200L))
            ),
          )
          if (enabled) assertMetrics(metrics, activeMetric(0L, start))
          else
            assertEquals(
              MetricExpectations.find(
                metrics,
                MetricExpectation.name(HttpExperimentalMetrics.ServerActiveRequests.name),
              ),
              None,
            )
        }
      }
    }
  }

  List(
    (Method.GET, Status.NotFound, true),
    (Method.HEAD, Status.ServiceUnavailable, false),
    (Method.GET, Status.Ok, false),
  )
    .foreach { case (method, status, isError) =>
      test(s"classification: response classifier receives request and response: $method $status") {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          var classifications = 0
          val config = ServerMetricsConfig.default.optIntoActiveRequests.withErrorClassifier {
            (request, response) =>
              classifications += 1
              request.method == Method.GET && response.status == Status.NotFound
          }
          for {
            middleware <- ServerMetrics.middleware[IO](kit, config)
            response <- middleware(HttpApp[IO](_ => IO.pure(Response[IO](status))))(
              request.withMethod(method)
            )
            _ <- response.body.compile.drain
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(classifications, 1)
            val labels = start.added(HttpAttributes.HttpRequestMethod(method.name))
            val terminal = labels ++ protocol ++ Attributes(
              HttpAttributes.HttpResponseStatusCode(status.code.toLong)
            ) ++
              Option.when(isError)(ErrorAttributes.ErrorType(status.code.toString))
            assertMetrics(metrics, activeMetric(0L, labels), durationMetric(terminal))
          }
        }
      }
    }

  test("classification: source exceptions take precedence over status classification") {
    val boom = new IllegalStateException("body error")
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val config = ServerMetricsConfig.default.optIntoActiveRequests
        .withErrorClassifier((_, _) => true)
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config)
        response <- middleware(
          HttpApp[IO](_ =>
            IO.pure(
              Response[IO](Status.ServiceUnavailable).withBodyStream(Stream.raiseError[IO](boom))
            )
          )
        )(request)
        result <- response.body.compile.drain.attempt
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(result, Left(boom))
        assertMetrics(
          metrics,
          activeMetric(0L, start),
          durationMetric(
            start ++ protocol ++ Attributes(
              HttpAttributes.HttpResponseStatusCode(503L),
              ErrorAttributes.ErrorType(boom.getClass.getName),
            )
          ),
        )
      }
    }
  }

}
