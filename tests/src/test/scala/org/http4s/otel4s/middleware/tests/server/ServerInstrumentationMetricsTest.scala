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
import cats.effect.Ref
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.ExemplarExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.StatusExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext

import scala.concurrent.duration._

class ServerInstrumentationMetricsTest extends CatsEffectSuite {
  private val request = Request[IO](uri = uri"https://example.test/items/42")
    .withHeaders(Headers("Content-Length" -> "3", "Host" -> "example.test:443"))
  private val provider = ServerSpanDataProvider.openTelemetry(
    new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {}
  )
  private val durationBuckets = BucketBoundaries(Vector(0d, 1d, 6d))
  private val requestBuckets = BucketBoundaries(Vector(0d, 1d, 4d))
  private val responseBuckets = BucketBoundaries(Vector(0d, 2d, 8d))
  private val initial = Attributes(
    Attribute("component", "shared-server"),
    Attribute("request.label", "present"),
    HttpAttributes.HttpRequestMethod("GET"),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
    UrlAttributes.UrlScheme("https"),
  )
  private val terminal = initial ++ Attributes(
    HttpAttributes.HttpRoute("/items/{id}"),
    HttpAttributes.HttpResponseStatusCode(503L),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

  List(
    (false, "full"),
    (true, "full"),
    (true, "partial"),
    (true, "caught-error"),
    (true, "cancel"),
    (true, "replay"),
  ).foreach { case (observe, mode) =>
    test(s"configured instruments, attributes and sizes: observe=$observe, mode=$mode") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val boom = new IllegalStateException("body")
          for {
            tracer <- kit.tracerProvider.get("test")
            entered <- Deferred[IO, Unit]
            http <- Ref.of[IO, Option[SpanContext]](None)
            wrap <- createMiddleware(kit, config(observe))
            requestBody = Stream
              .sleep_[IO](1.second) ++ Stream.emits(Vector[Byte](1, 2, 3)).covary[IO]
            responseBody = (Stream.sleep_[IO](2.seconds) ++
              (if (mode == "cancel") Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
               else Stream.emits(Vector[Byte](1, 2, 3, 4)).covary[IO]) ++
              (if (mode == "caught-error") Stream.raiseError[IO](boom) else Stream.empty))
              .onFinalize(IO.sleep(1.second))
            base = HttpApp[IO](req =>
              IO.sleep(1.second) >> req.body.compile.drain >>
                tracer.currentSpanContext
                  .flatMap(http.set)
                  .as(
                    Response[IO](Status.ServiceUnavailable)
                      .withHeaders(Headers("Content-Length" -> "4"))
                      .withBodyStream(responseBody)
                  )
            )
            response <- wrap(base)(request.withBodyStream(requestBody))
            _ <- mode match {
              case "partial" => response.body.take(1).compile.drain
              case "caught-error" => response.body.handleErrorWith(_ => Stream.empty).compile.drain
              case "cancel" =>
                for {
                  fiber <- response.body.compile.drain.start
                  _ <- entered.get
                  _ <- fiber.cancel
                } yield ()
              case "replay" => response.body.compile.drain >> response.body.compile.drain
              case _ => response.body.compile.drain
            }
            metrics <- kit.collectMetrics
            context <- http.get.map(_.get)
            spans <- kit.finishedSpans
          } yield {
            val labels = terminal ++ Option.when(mode == "caught-error")(
              ErrorAttributes.ErrorType(boom.getClass.getName)
            )
            val responseSize = Option.when(!observe || Set("full", "replay")(mode))(4d)
            val expectations = List(
              MetricExpectation
                .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                .exactlyPoints(PointExpectation.numeric(0L).attributesExact(initial)),
              histogram(
                HttpMetrics.ServerRequestDuration.name,
                "s",
                5d,
                durationBuckets,
                labels,
                context,
              ),
              histogram(
                HttpExperimentalMetrics.ServerRequestBodySize.name,
                "By",
                3d,
                requestBuckets,
                labels,
                context,
              ),
            ) ++ responseSize.map(size =>
              histogram(
                HttpExperimentalMetrics.ServerResponseBodySize.name,
                "By",
                size,
                responseBuckets,
                labels,
                context,
              )
            )
            MetricExpectations
              .checkAllDistinct(metrics, expectations: _*)
              .fold(m => fail(MetricExpectations.format(m)), identity)
            if (responseSize.isEmpty)
              assertEquals(
                MetricExpectations.find(
                  metrics,
                  MetricExpectation.name(HttpExperimentalMetrics.ServerResponseBodySize.name),
                ),
                None,
              )
            TraceExpectations
              .check(
                spans,
                TraceForestExpectation.unordered(
                  TraceExpectation.leaf(
                    SpanExpectation
                      .server("GET")
                      .spanContextExact(context)
                      .status(StatusExpectation.error)
                      .eventCount(if (mode == "caught-error") 1 else 0)
                      .where("span shares the supplied duration") { span =>
                        span.endTimestamp.contains(span.startTimestamp + 5.seconds)
                      }
                  )
                ),
              )
              .fold(m => fail(TraceExpectations.format(m)), identity)
          }
        }
      }
    }
  }

  private def histogram(
      name: String,
      unit: String,
      value: Double,
      buckets: BucketBoundaries,
      attributes: Attributes,
      context: SpanContext,
  ): MetricExpectation =
    MetricExpectation
      .histogram(name)
      .unit(unit)
      .exactlyPoints(
        PointExpectation.histogram
          .count(1L)
          .sum(value)
          .boundaries(buckets)
          .attributesExact(attributes)
          .exemplarCount(1)
          .containsExemplars(
            ExemplarExpectation
              .numeric(value)
              .traceContext(TraceContext(context.traceId, context.spanId, context.isSampled))
          )
      )
  private def createMiddleware(
      kit: OpenTelemetrySdkTestkit[IO],
      config: ServerMetricsConfig,
  ): IO[HttpApp[IO] => HttpApp[IO]] =
    ServerInstrumentation.middleware[IO](kit, config, ServerTracingConfig(provider))

  private def config(observe: Boolean): ServerMetricsConfig =
    ServerMetricsConfig.default.optIntoActiveRequests
      .withAttributes(Attributes(Attribute("component", "shared-server")))
      .withRequestAttributes(_ => Attributes(Attribute("request.label", "present")))
      .optIntoServerAddress
      .optIntoServerPort
      .withRouteClassifier(_ => Some("/items/{id}"))
      .withErrorClassifier(ErrorClassifier.never)
      .withResponseDurationSecondsHistogramBuckets(durationBuckets)
      .optIntoRequestBodySize(
        if (observe) BodySizeConfig.observe(requestBuckets)
        else BodySizeConfig.contentLength(requestBuckets)
      )
      .optIntoResponseBodySize(
        if (observe) BodySizeConfig.observe(responseBuckets)
        else BodySizeConfig.contentLength(responseBuckets)
      )

}
