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

import cats.effect.Deferred
import cats.effect.IO
import fs2.Stream
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.ExemplarExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.SpanKind

class ServerBodySizeExemplarTest extends CatsEffectSuite {
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("POST"), UrlAttributes.UrlScheme("https"))
  private val terminal = start ++ Attributes(
    HttpAttributes.HttpResponseStatusCode(200L),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )
  private val buckets = BucketBoundaries(0d, 3d, 7d, 15d)
  private val request = Request[IO](Method.POST, uri"https://example.test/items")
    .withBodyStream(Stream.emits(Vector[Byte](1, 2, 3)).covary[IO])
    .putHeaders(Header.Raw(ci"Content-Length", "9"))

  List(
    (false, "success"),
    (true, "success"),
    (false, "error"),
    (true, "error"),
    (true, "cancel"),
    (true, "replay"),
    (true, "unstarted"),
  ).foreach { case (observed, mode) =>
    test(s"size exemplars are recorded once before the HTTP span ends: observe=$observed $mode") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        val sizeConfig =
          if (observed) BodySizeConfig.observe(buckets) else BodySizeConfig.contentLength(buckets)
        val config = ServerMetricsConfig.default.optIntoActiveRequests
          .optIntoRequestBodySize(sizeConfig)
          .optIntoResponseBodySize(sizeConfig)
        val boom = new IllegalStateException("response body")
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config)
          tracer <- kit.tracerProvider.get("test")
          entered <- Deferred[IO, Unit]
          _ <- tracer.spanBuilder("HTTP").withSpanKind(SpanKind.Server).build.use { http =>
            val body = Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO] ++
              (if (mode == "error") Stream.raiseError[IO](boom)
               else if (mode == "cancel")
                 Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
               else Stream.empty)
            for {
              response <- middleware(
                HttpApp[IO](req =>
                  req.body.compile.drain.as(
                    Response[IO]()
                      .withBodyStream(body)
                      .putHeaders(Header.Raw(ci"Content-Length", "11"))
                  )
                )
              )(request)
              before <- kit.collectMetrics
              _ = List(
                HttpExperimentalMetrics.ServerRequestBodySize.name,
                HttpExperimentalMetrics.ServerResponseBodySize.name,
                HttpMetrics.ServerRequestDuration.name,
              ).foreach { name =>
                assertEquals(MetricExpectations.find(before, MetricExpectation.name(name)), None)
              }
              _ <- tracer.span("consumer").use { consumer =>
                val consume = mode match {
                  case "unstarted" => IO.unit
                  case "error" =>
                    response.body.compile.drain.attempt
                      .flatMap(result => IO(assertEquals(result, Left(boom))))
                  case "cancel" =>
                    for {
                      fiber <- response.body.compile.drain.start
                      _ <- entered.get
                      _ <- fiber.cancel
                    } yield ()
                  case "replay" => response.body.compile.drain >> response.body.compile.drain
                  case _ => response.body.compile.drain
                }
                consume >> tracer.currentSpanContext
                  .map(current => assertEquals(current, Some(consumer.context)))
              }
              metrics <- kit.collectMetrics
              spans <- kit.finishedSpans
              recording <- http.isRecording
            } yield {
              assert(recording)
              assertEquals(SpanExpectations.find(spans, SpanExpectation.name("HTTP")), None)
              if (mode == "unstarted") {
                List(
                  HttpExperimentalMetrics.ServerRequestBodySize.name,
                  HttpExperimentalMetrics.ServerResponseBodySize.name,
                  HttpMetrics.ServerRequestDuration.name,
                ).foreach { name =>
                  assertEquals(
                    MetricExpectations.find(metrics, MetricExpectation.name(name)),
                    None,
                  )
                }
                assertMetrics(
                  metrics,
                  MetricExpectation
                    .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                    .exactlyPoints(PointExpectation.numeric(1L).attributesExact(start)),
                )
              } else {
                val labels = terminal ++ Option.when(mode == "error")(
                  ErrorAttributes.ErrorType(boom.getClass.getName)
                )
                assertMetrics(
                  metrics,
                  sizeExpectation(
                    HttpExperimentalMetrics.ServerRequestBodySize.name,
                    if (observed) 3L else 9L,
                    labels,
                    http.context,
                  ),
                  MetricExpectation
                    .histogram(HttpMetrics.ServerRequestDuration.name)
                    .exactlyPoints(
                      PointExpectation.histogram
                        .count(1L)
                        .attributesExact(labels)
                        .exemplarCount(1)
                    ),
                  MetricExpectation
                    .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                    .exactlyPoints(PointExpectation.numeric(0L).attributesExact(start)),
                )
                if (observed && Set("error", "cancel")(mode))
                  assertEquals(
                    MetricExpectations.find(
                      metrics,
                      MetricExpectation.name(HttpExperimentalMetrics.ServerResponseBodySize.name),
                    ),
                    None,
                  )
                else
                  assertMetrics(
                    metrics,
                    sizeExpectation(
                      HttpExperimentalMetrics.ServerResponseBodySize.name,
                      if (observed) 5L else 11L,
                      labels,
                      http.context,
                    ),
                  )
              }
            }
          }
        } yield ()
      }
    }
  }

  private def sizeExpectation(
      name: String,
      value: Long,
      attributes: Attributes,
      context: SpanContext,
  ): MetricExpectation =
    MetricExpectation
      .histogram(name)
      .unit("By")
      .exactlyPoints(
        PointExpectation.histogram
          .count(1L)
          .sum(value.toDouble)
          .boundaries(buckets)
          .attributesExact(attributes)
          .exemplarCount(1)
          .containsExemplars(
            ExemplarExpectation
              .numeric(value.toDouble)
              .traceContext(TraceContext(context.traceId, context.spanId, context.isSampled))
          )
      )

  private def assertMetrics(metrics: List[MetricData], expectations: MetricExpectation*)(implicit
      loc: Location
  ): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
}
