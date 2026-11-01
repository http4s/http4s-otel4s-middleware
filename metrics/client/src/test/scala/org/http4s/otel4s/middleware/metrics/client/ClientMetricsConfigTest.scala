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
import cats.effect.Resource
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Header
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.syntax.literals._
import org.typelevel.ci.CIString
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
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ClientMetricsConfigTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items/123")

  List(
    (Method.GET, Status.NotFound, false),
    (Method.GET, Status.ServiceUnavailable, true),
    (Method.HEAD, Status.ServiceUnavailable, false),
    (Method.GET, Status.Ok, true),
  ).foreach { case (method, status, isError) =>
    test(s"classification: response classifier receives request and response: $method $status") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        var evaluations = 0
        for {
          middleware <- ClientMetrics.middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests
              .withErrorClassifier { (req, resp) =>
                evaluations += 1
                req.method == Method.GET && Set(Status.Ok, Status.ServiceUnavailable)(resp.status)
              },
          )
          client = middleware(Client[IO](_ => Resource.pure(Response[IO](status))))
          _ <- client.run(request.withMethod(method)).use(_.body.compile.drain)
          metrics <- testkit.collectMetrics
        } yield {
          assertEquals(evaluations, 1)
          val error = Option.when(isError)(ErrorAttributes.ErrorType(status.code.toString))
          assertMetrics(
            metrics,
            activeMetric(0L, start(method)),
            durationMetric(start(method) ++ response(status) ++ error),
          )
        }
      }
    }
  }

  List(false, true).foreach { classifiedError =>
    test(
      s"classification: body exception takes precedence over configured status classification: $classifiedError"
    ) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        val boom = new IllegalStateException("body")
        for {
          middleware <- ClientMetrics.middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests
              .withErrorClassifier((_, _) => classifiedError),
          )
          client = middleware(
            Client[IO](_ =>
              Resource.pure(
                Response[IO](Status.ServiceUnavailable)
                  .withBodyStream(Stream.raiseError[IO](boom))
              )
            )
          )
          result <- client.run(request).use(_.body.compile.drain).attempt
          metrics <- testkit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          assertMetrics(
            metrics,
            activeMetric(0L, start()),
            durationMetric(
              start() ++ response(Status.ServiceUnavailable)
                .added(ErrorAttributes.ErrorType(boom.getClass.getName))
            ),
          )
        }
      }
    }
  }

  test("immutability: derived config does not change the original response classifier") {
    val original = ClientMetricsConfig.default.optIntoActiveRequests
    val derived = original.withErrorClassifier(ErrorClassifier.default.excluding(Status.NotFound))
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      for {
        defaultMiddleware <- ClientMetrics.middleware[IO](testkit, original)
        customMiddleware <- ClientMetrics.middleware[IO](testkit, derived)
        base = Client[IO](_ => Resource.pure(Response[IO](Status.NotFound)))
        _ <- defaultMiddleware(base).run(request).use(_.body.compile.drain)
        _ <- customMiddleware(base).run(request).use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield assertMetrics(
        metrics,
        activeMetric(0L, start()),
        MetricExpectation
          .histogram(HttpMetrics.ClientRequestDuration.name)
          .exactlyPoints(
            PointExpectation.histogram
              .count(1L)
              .attributesExact(start() ++ response(Status.NotFound)),
            PointExpectation.histogram
              .count(1L)
              .attributesExact(
                (start() ++ response(Status.NotFound))
                  .added(ErrorAttributes.ErrorType("404"))
              ),
          ),
      )
    }
  }

  test("reuse: one config can be reused across independent telemetry instances") {
    val tag = Attribute("component", "shared-config")
    val config = ClientMetricsConfig.default.optIntoActiveRequests.withAttributes(Attributes(tag))
    val kit = OpenTelemetrySdkTestkit.inMemory[IO]()
    kit.use { first =>
      kit.use { second =>
        for {
          firstMiddleware <- ClientMetrics.middleware[IO](first, config)
          secondMiddleware <- ClientMetrics.middleware[IO](second, config)
          base = Client[IO](_ => Resource.pure(Response[IO]()))
          _ <- firstMiddleware(base).run(request).use(_.body.compile.drain)
          _ <- firstMiddleware(base).run(request).use(_.body.compile.drain)
          _ <- secondMiddleware(base).run(request).use(_.body.compile.drain)
          firstMetrics <- first.collectMetrics
          secondMetrics <- second.collectMetrics
        } yield {
          val labels = start().added(tag)
          val terminal = labels ++ response(Status.Ok)
          assertMetrics(
            firstMetrics,
            activeMetric(0L, labels),
            MetricExpectation
              .histogram(HttpMetrics.ClientRequestDuration.name)
              .exactlyPoints(PointExpectation.histogram.count(2L).attributesExact(terminal)),
          )
          assertMetrics(secondMetrics, activeMetric(0L, labels), durationMetric(terminal))
        }
      }
    }
  }

  test("buckets: configured duration buckets reach the recorded histogram") {
    val buckets = BucketBoundaries(Vector(.1, .5, 2d))
    val original = ClientMetricsConfig.default.optIntoActiveRequests
    val configured = original.withResponseDurationSecondsHistogramBuckets(buckets)
    assertNotEquals(original.responseDurationSecondsHistogramBuckets, buckets)
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      for {
        middleware <- ClientMetrics.middleware[IO](testkit, configured)
        client = middleware(Client[IO](_ => Resource.pure(Response[IO]())))
        _ <- client.run(request).use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield assertMetrics(
        metrics,
        activeMetric(0L, start()),
        MetricExpectation
          .histogram(HttpMetrics.ClientRequestDuration.name)
          .exactlyPoints(
            PointExpectation.histogram
              .count(1L)
              .boundaries(buckets)
              .attributesExact(start() ++ response(Status.Ok))
          ),
      )
    }
  }

  test("URL scheme: opting in repeatedly is idempotent") {
    val config = ClientMetricsConfig.default.optIntoActiveRequests.optIntoUrlScheme.optIntoUrlScheme
    assert(config.urlSchemeEnabled)
    assert(!ClientMetricsConfig.default.optIntoActiveRequests.urlSchemeEnabled)
  }

  List(
    ("disabled by default", ClientMetricsConfig.default, false),
    ("enabled explicitly", ClientMetricsConfig.default.optIntoActiveRequests, true),
    (
      "repeated opt-in",
      ClientMetricsConfig.default.optIntoActiveRequests.optIntoActiveRequests,
      true,
    ),
  ).foreach { case (name, config, enabled) =>
    test(s"active requests: $name") {
      assertEquals(config.activeRequestsEnabled, enabled)
      assert(!ClientMetricsConfig.default.activeRequestsEnabled)
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        for {
          middleware <- ClientMetrics.middleware[IO](testkit, config)
          client = middleware(Client[IO](_ => Resource.pure(Response[IO]())))
          _ <- client.run(request).use(_.body.compile.drain)
          metrics <- testkit.collectMetrics
        } yield {
          val duration = durationMetric(start() ++ response(Status.Ok))
          if (enabled) assertMetrics(metrics, activeMetric(0L, start()), duration)
          else {
            assertMetrics(metrics, duration)
            assertEquals(
              MetricExpectations.find(
                metrics,
                MetricExpectation.name(HttpExperimentalMetrics.ClientActiveRequests.name),
              ),
              None,
            )
          }
        }
      }
    }
  }

  test("body-size settings are immutable and request/response opt-ins are independent") {
    val original = ClientMetricsConfig.default
    val requestSize = BodySizeConfig.observe(BucketBoundaries(0d, 3d, 7d))
    val responseSize = BodySizeConfig.contentLength(BucketBoundaries(0d, 10d, 100d))
    val configured =
      original.optIntoRequestBodySize(requestSize).optIntoResponseBodySize(responseSize)
    assertEquals(original.requestBodySize, None)
    assertEquals(original.responseBodySize, None)
    assertEquals(configured.requestBodySize, Some(requestSize))
    assertEquals(configured.responseBodySize, Some(responseSize))

    val resetRequest = configured.optIntoRequestBodySize
    assertEquals(resetRequest.requestBodySize, Some(BodySizeConfig.contentLength))
    assertEquals(resetRequest.responseBodySize, configured.responseBodySize)
    val resetResponse = configured.optIntoResponseBodySize
    assertEquals(resetResponse.responseBodySize, Some(BodySizeConfig.contentLength))
    assertEquals(resetResponse.requestBodySize, configured.requestBodySize)
    assertEquals(BodySizeConfig.observe.bucketBoundaries, BodySizeConfig.DefaultBucketBoundaries)
  }

  test("default body-size opt-ins record declared lengths with default byte boundaries") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      val config = ClientMetricsConfig.default.optIntoRequestBodySize.optIntoResponseBodySize
      val req = request.putHeaders(Header.Raw(CIString("Content-Length"), "3"))
      val resp = Response[IO]().putHeaders(Header.Raw(CIString("Content-Length"), "5"))
      for {
        middleware <- ClientMetrics.middleware[IO](testkit, config)
        _ <- middleware(Client[IO](_ => Resource.pure(resp))).run(req).use(_ => IO.unit)
        metrics <- testkit.collectMetrics
      } yield {
        val labels = start() ++ response(Status.Ok)
        def size(name: String, bytes: Double): MetricExpectation =
          MetricExpectation
            .histogram(name)
            .unit("By")
            .exactlyPoints(
              PointExpectation.histogram
                .count(1L)
                .sum(bytes)
                .boundaries(BodySizeConfig.DefaultBucketBoundaries)
                .attributesExact(labels)
            )
        assertMetrics(
          metrics,
          durationMetric(labels),
          size(HttpExperimentalMetrics.ClientRequestBodySize.name, 3d),
          size(HttpExperimentalMetrics.ClientResponseBodySize.name, 5d),
        )
      }
    }
  }

  private def start(method: Method = Method.GET): Attributes = Attributes(
    HttpAttributes.HttpRequestMethod(method.name),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  private def response(status: Status): Attributes = Attributes(
    HttpAttributes.HttpResponseStatusCode(status.code.toLong),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

}
