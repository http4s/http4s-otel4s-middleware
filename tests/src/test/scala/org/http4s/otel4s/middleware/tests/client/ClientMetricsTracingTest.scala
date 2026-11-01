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
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.ExemplarExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration._

class ClientMetricsTracingTest extends CatsEffectSuite {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val custom = Attribute("test.tag", "present")
  private val startAttributes = Attributes(
    custom,
    HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  test(
    "deferred recording restores the acquisition span for its exemplar and restores consumer context"
  ) {
    TestControl.executeEmbed {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        for {
          middleware <- ClientMetrics.middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests.withAttributes(Attributes(custom)),
          )
          tracer <- testkit.tracerProvider.get("test")
          base = Client[IO](_ =>
            Resource.pure(Response[IO]().withBodyStream(Stream.sleep_[IO](1.second)))
          )
          http <- tracer.spanBuilder("HTTP").withSpanKind(SpanKind.Client).build.resource.use { res =>
            middleware(base)
              .run(request)
              .mapK(res.trace)
              .use { response =>
                tracer.span("consumer").use { consumer =>
                  response.body.compile.drain >> tracer.currentSpanContext.map { current =>
                    assertEquals(current, Some(consumer.context))
                  }
                }
              }
              .as(res.span.context)
          }
          metrics <- testkit.collectMetrics
        } yield assertMetrics(
          metrics,
          active(0L),
          MetricExpectation
            .histogram(HttpMetrics.ClientRequestDuration.name)
            .exactlyPoints(
              PointExpectation.histogram
                .count(1L)
                .sum(1.0)
                .attributesExact(terminal(Status.Ok))
                .exemplarCount(1)
                .containsExemplars(
                  ExemplarExpectation
                    .numeric(1.0)
                    .traceContext(TraceContext(http.traceId, http.spanId, http.isSampled))
                )
            ),
        )
      }
    }
  }

  List((true, false), (true, true), (false, false), (false, true)).foreach {
    case (tracingOutside, unread) =>
      test(s"existing tracing composition: tracingOutside=$tracingOutside, unread=$unread") {
        TestControl.executeEmbed {
          OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
            implicit val tracerProvider: TracerProvider[IO] = testkit.tracerProvider
            for {
              middleware <- ClientMetrics.middleware[IO](
                testkit,
                ClientMetricsConfig.default.optIntoActiveRequests.withAttributes(Attributes(custom)),
              )
              tracing <- ClientMiddleware
                .builder[IO](
                  ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
                )
                .build
              tracer <- testkit.tracerProvider.get("test")
              http <- Ref.of[IO, Option[SpanContext]](None)
              base = Client[IO](_ =>
                Resource.eval(
                  tracer.currentSpanContext
                    .flatMap(http.set)
                    .as(Response[IO]().withBodyStream(Stream.sleep_[IO](1.second)))
                )
              )
              parent <- tracer.span("parent").use { parent =>
                val client =
                  if (tracingOutside) tracing.wrapClient(middleware(base))
                  else middleware(tracing.wrapClient(base))
                client
                  .run(request)
                  .use { response =>
                    tracer.span("consumer").use { consumer =>
                      val consume =
                        if (unread) IO.sleep(1.second) else response.body.compile.drain
                      consume >> tracer.currentSpanContext
                        .map(current => assertEquals(current, Some(consumer.context)))
                    }
                  }
                  .as(parent.context)
              }
              context <- http.get.map(_.get)
              metrics <- testkit.collectMetrics
              spans <- testkit.finishedSpans
            } yield {
              val expected = if (tracingOutside) context else parent
              assertNotEquals(context.spanId, parent.spanId)
              assertMetrics(
                metrics,
                active(0L),
                MetricExpectation
                  .histogram(HttpMetrics.ClientRequestDuration.name)
                  .exactlyPoints(
                    PointExpectation.histogram
                      .count(1L)
                      .sum(1.0)
                      .attributesExact(terminal(Status.Ok))
                      .exemplarCount(1)
                      .containsExemplars(
                        ExemplarExpectation
                          .numeric(1.0)
                          .traceContext(
                            TraceContext(expected.traceId, expected.spanId, expected.isSampled)
                          )
                      )
                  ),
              )
              TraceExpectations
                .check(
                  spans,
                  TraceForestExpectation.unordered(
                    TraceExpectation.unordered(
                      SpanExpectation.name("parent"),
                      TraceExpectation
                        .leaf(SpanExpectation.name("GET").spanContextExact(context)),
                      TraceExpectation.leaf(SpanExpectation.name("consumer")),
                    )
                  ),
                )
                .fold(mismatches => fail(TraceExpectations.format(mismatches)), identity)
            }
          }
        }
      }
  }

  private def terminal(status: Status): Attributes =
    startAttributes ++ Attributes(
      HttpAttributes.HttpResponseStatusCode(status.code.toLong),
      NetworkAttributes.NetworkProtocolVersion("1.1"),
    )

  private def active(value: Long): MetricExpectation =
    MetricExpectation
      .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
      .exactlyPoints(PointExpectation.numeric(value).attributesExact(startAttributes))

  private def assertMetrics(metrics: List[MetricData], expectations: MetricExpectation*): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
}
