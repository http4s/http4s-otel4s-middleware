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

package org.http4s.otel4s.middleware.tests.server

import cats.effect.IO
import cats.effect.testkit.TestControl
import com.comcast.ip4s._
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.semconv.attributes.ClientAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.attributes.UserAgentAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ServerInstrumentationAttributesTest extends CatsEffectSuite {
  List(false, true).foreach { metricOptIns =>
    List(false, true).foreach { traceOptIns =>
      test(s"metric and span opt-ins stay separate: metrics=$metricOptIns, tracing=$traceOptIns") {
        TestControl.executeEmbed {
          OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
            val baseProvider = ServerSpanDataProvider.openTelemetry(
              new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {}
            )
            val provider =
              if (traceOptIns)
                baseProvider.optIntoClientPort
                  .optIntoHttpRequestHeaders(
                    HeaderRedactor(Set(ci"x-request"), HeaderRedactor.Behavior.Elide)
                  )
                  .optIntoHttpResponseHeaders(
                    HeaderRedactor(Set(ci"x-response"), HeaderRedactor.Behavior.Elide)
                  )
              else baseProvider
            val config =
              if (metricOptIns)
                ServerMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize.optIntoServerAddress.optIntoServerPort
              else ServerMetricsConfig.default
            val request = Request[IO](uri = uri"/items")
              .putHeaders(
                "Host" -> "example.test",
                "Content-Length" -> "3",
                "User-Agent" -> "test-client",
                "x-request" -> "request-value",
              )
              .withAttribute(
                Request.Keys.ConnectionInfo,
                Request.Connection(
                  SocketAddress(ip"127.0.0.1", port"8443"),
                  SocketAddress(ip"192.0.2.1", port"12345"),
                  secure = true,
                ),
              )
              .withBodyStream(Stream.emits(Vector[Byte](1, 2, 3)).covary[IO])
            for {
              wrap <- ServerInstrumentation
                .middleware[IO](kit, config, ServerTracingConfig(provider))
              response <- wrap(
                HttpApp[IO](req =>
                  req.body.compile.drain.as(
                    Response[IO]()
                      .putHeaders("Content-Length" -> "4", "x-response" -> "response-value")
                      .withBodyStream(Stream.emits(Vector[Byte](1, 2, 3, 4)).covary[IO])
                  )
                )
              )(request)
              _ <- response.body.compile.drain
              metrics <- kit.collectMetrics
              spans <- kit.finishedSpans
            } yield {
              val start = Attributes(
                HttpAttributes.HttpRequestMethod("GET"),
                UrlAttributes.UrlScheme("https"),
              ) ++
                (if (metricOptIns)
                   Attributes(
                     ServerAttributes.ServerAddress("example.test"),
                     ServerAttributes.ServerPort(443L),
                   )
                 else Attributes.empty)
              val terminal = start ++ Attributes(
                HttpAttributes.HttpResponseStatusCode(200L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
              )
              def histogram(name: String, unit: String, value: Double): MetricExpectation =
                MetricExpectation
                  .histogram(name)
                  .unit(unit)
                  .exactlyPoints(
                    PointExpectation.histogram.count(1L).sum(value).attributesExact(terminal)
                  )
              val duration = histogram(HttpMetrics.ServerRequestDuration.name, "s", 0d)
              val optional = List(
                MetricExpectation
                  .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                  .unit("{request}")
                  .exactlyPoints(PointExpectation.numeric(0L).attributesExact(start)),
                histogram(HttpExperimentalMetrics.ServerRequestBodySize.name, "By", 3d),
                histogram(HttpExperimentalMetrics.ServerResponseBodySize.name, "By", 4d),
              )
              MetricExpectations
                .checkAllDistinct(
                  metrics,
                  (List(duration) ++ (if (metricOptIns) optional else Nil)): _*
                )
                .fold(m => fail(MetricExpectations.format(m)), identity)
              if (!metricOptIns)
                List(
                  HttpExperimentalMetrics.ServerActiveRequests.name,
                  HttpExperimentalMetrics.ServerRequestBodySize.name,
                  HttpExperimentalMetrics.ServerResponseBodySize.name,
                ).foreach { name =>
                  assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(name)), None)
                }
              val attributes = Attributes(
                HttpAttributes.HttpRequestMethod("GET"),
                HttpAttributes.HttpResponseStatusCode(200L),
                UrlAttributes.UrlPath("/items"),
                UrlAttributes.UrlScheme("https"),
                ClientAttributes.ClientAddress("192.0.2.1"),
                NetworkAttributes.NetworkPeerAddress("192.0.2.1"),
                NetworkAttributes.NetworkPeerPort(12345L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                ServerAttributes.ServerAddress("example.test"),
                ServerAttributes.ServerPort(443L),
                UserAgentAttributes.UserAgentOriginal("test-client"),
              ) ++ (if (traceOptIns)
                      Attributes(
                        ClientAttributes.ClientPort(12345L),
                        HttpAttributes.HttpRequestHeader
                          .transformName(_ + ".x-request")(Seq("request-value")),
                        HttpAttributes.HttpResponseHeader
                          .transformName(_ + ".x-response")(Seq("response-value")),
                      )
                    else Attributes.empty)
              SpanExpectations
                .checkAllDistinct(spans, SpanExpectation.server("GET").attributesExact(attributes))
                .fold(m => fail(SpanExpectations.format(m)), identity)
            }
          }
        }
      }
    }
  }
}
