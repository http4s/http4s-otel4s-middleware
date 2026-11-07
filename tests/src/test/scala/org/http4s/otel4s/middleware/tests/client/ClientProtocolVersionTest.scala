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

package org.http4s.otel4s.middleware.tests.client

import cats.effect.IO
import cats.effect.Resource
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpVersion
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.syntax.literals._
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.TracerProvider

class ClientProtocolVersionTest extends CatsEffectSuite {
  List(
    (HttpVersion.`HTTP/1.1`, HttpVersion.`HTTP/1.0`, "1.0"),
    (HttpVersion.`HTTP/1.1`, HttpVersion.`HTTP/1.1`, "1.1"),
    (HttpVersion.`HTTP/1.1`, HttpVersion.`HTTP/2`, "2"),
    (HttpVersion.`HTTP/1.1`, HttpVersion.`HTTP/3`, "3"),
    (HttpVersion.`HTTP/2`, HttpVersion.`HTTP/1.1`, "1.1"),
  ).foreach { case (requested, actual, expected) =>
    test(s"standalone client records the response protocol: $requested -> $actual") {
      checkVersion(
        requested,
        IO.pure(Response[IO](httpVersion = actual)),
        Some(expected),
        combined = false,
      )
    }

    test(s"combined client records the response protocol: $requested -> $actual") {
      checkVersion(
        requested,
        IO.pure(Response[IO](httpVersion = actual)),
        Some(expected),
        combined = true,
      )
    }
  }

  test("standalone client omits the protocol when acquisition fails") {
    val error = new RuntimeException("acquisition")
    checkVersion(
      HttpVersion.`HTTP/2`,
      IO.raiseError(error),
      None,
      combined = false,
      expectedError = Some(error),
    )
  }

  test("combined client omits the protocol when acquisition fails") {
    val error = new RuntimeException("acquisition")
    checkVersion(
      HttpVersion.`HTTP/2`,
      IO.raiseError(error),
      None,
      combined = true,
      expectedError = Some(error),
    )
  }

  test("standalone client retains the response protocol after a body failure") {
    val error = new RuntimeException("body")
    val response = Response[IO](httpVersion = HttpVersion.`HTTP/2`)
      .withBodyStream(Stream.raiseError[IO](error))
    checkVersion(
      HttpVersion.`HTTP/1.1`,
      IO.pure(response),
      Some("2"),
      combined = false,
      expectedError = Some(error),
    )
  }

  test("combined client retains the response protocol after a body failure") {
    val error = new RuntimeException("body")
    val response = Response[IO](httpVersion = HttpVersion.`HTTP/2`)
      .withBodyStream(Stream.raiseError[IO](error))
    checkVersion(
      HttpVersion.`HTTP/1.1`,
      IO.pure(response),
      Some("2"),
      combined = true,
      expectedError = Some(error),
    )
  }

  private def checkVersion(
      requested: HttpVersion,
      response: IO[Response[IO]],
      expected: Option[String],
      combined: Boolean,
      expectedError: Option[Throwable] = None,
  ): IO[Unit] = OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
    implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
    val provider = ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
    val config = ClientMetricsConfig.default
    val wrap =
      if (combined)
        ClientInstrumentation.middleware[IO](kit, config, ClientTracingConfig(provider))
      else
        for {
          metrics <- ClientMetrics.middleware[IO](kit, config)
          tracing <- ClientMiddleware.builder[IO](provider).build
        } yield (client: Client[IO]) => tracing.wrapClient(metrics(client))
    val base = Client[IO](_ => Resource.eval(response))
    for {
      middleware <- wrap
      result <- middleware(base)
        .run(Request[IO](uri = uri"https://example.test/items", httpVersion = requested))
        .use(_.body.compile.drain)
        .attempt
      metrics <- kit.collectMetrics
      spans <- kit.finishedSpans
    } yield {
      assertEquals(result, expectedError.toLeft(()))
      MetricExpectations
        .checkAllDistinct(
          metrics,
          MetricExpectation
            .histogram(HttpMetrics.ClientRequestDuration.name)
            .exactlyPoints(PointExpectation.histogram.count(1L))
            .where("metric protocol comes from the response, if available") { metric =>
              metric.data.points.forall(
                _.attributes.get(NetworkAttributes.NetworkProtocolVersion).map(_.value) == expected
              )
            },
        )
        .fold(m => fail(MetricExpectations.format(m)), identity)
      SpanExpectations
        .checkAllDistinct(
          spans,
          SpanExpectation
            .client("GET")
            .where("span protocol matches the response and duration metric") { span =>
              span.attributes.elements
                .get(NetworkAttributes.NetworkProtocolVersion)
                .map(_.value) == expected
            },
        )
        .fold(m => fail(SpanExpectations.format(m)), identity)
      assertEquals(spans.size, 1)
    }
  }
}
