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
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.ExemplarExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.EventExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.sdk.testkit.trace.StatusExpectation
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.ExceptionAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration._

/** Checks recording and span completion for app and response body outcomes. */
class ServerInstrumentationLifecycleTest extends CatsEffectSuite {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val cases = List(
    "full",
    "partial",
    "unstarted",
    "body-error",
    "caught-body-error",
    "body-cleanup-error",
    "body-cancel",
    "consumer-error-before-body",
    "consumer-error-after-body",
    "error-response",
    "error-response-body-error",
    "app-error",
    "app-cancel",
    "replay",
    "trace-classifier-never",
  )

  cases.foreach { mode =>
    test(s"server completion: $mode") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val appFailure = mode.startsWith("app-")
          val unstarted = Set("unstarted", "consumer-error-before-body")(mode)
          val sourceError = Set(
            "body-error",
            "caught-body-error",
            "body-cleanup-error",
            "error-response-body-error",
            "app-error",
          )(mode)
          val errorResponse = mode.startsWith("error-response") || mode.endsWith("classifier-never")
          val status = if (errorResponse) Status.ServiceUnavailable else Status.Ok
          val boom = new IllegalStateException("failure")
          for {
            middleware <- createMiddleware(
              kit,
              metricsConfig =
                ServerMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize,
              tracingErrors =
                if (mode == "trace-classifier-never") ErrorClassifier.never
                else ErrorClassifier.serverError,
            )
            tracer <- kit.tracerProvider.get("test")
            http <- Ref.of[IO, Option[SpanContext]](None)
            entered <- Deferred[IO, Unit]
            body = (Stream.sleep_[IO](3.seconds) ++
              (if (mode == "body-cancel")
                 Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
               else Stream.emits(Vector[Byte](1, 2)).covary[IO]) ++
              (if (Set("body-error", "caught-body-error", "error-response-body-error")(mode))
                 Stream.raiseError[IO](boom)
               else Stream.empty))
              .onFinalize(
                IO.sleep(5.seconds) >>
                  (if (mode == "body-cleanup-error") IO.raiseError[Unit](boom) else IO.unit)
              )
            base = HttpApp[IO](_ =>
              tracer.currentSpanContext.flatMap(http.set) >> IO.sleep(2.seconds) >>
                (mode match {
                  case "app-error" => IO.raiseError[Response[IO]](boom)
                  case "app-cancel" => entered.complete(()).void >> IO.never[Response[IO]]
                  case _ =>
                    IO.pure(
                      Response[IO](status).withBodyStream(body).putHeaders("Content-Length" -> "2")
                    )
                })
            )
            run = middleware(base)(request.putHeaders("Content-Length" -> "4"))
              .flatMap { response =>
                // App return must leave the HTTP span open.
                kit.finishedSpans.flatMap { spans =>
                  IO(
                    assertEquals(SpanExpectations.find(spans, SpanExpectation.server("GET")), None)
                  ) >>
                    (mode match {
                      case "unstarted" => IO.unit
                      case "consumer-error-before-body" => IO.raiseError[Unit](boom)
                      case "replay" => response.body.compile.drain >> response.body.compile.drain
                      case "partial" => response.body.take(1).compile.drain
                      case "caught-body-error" =>
                        response.body.handleErrorWith(_ => Stream.empty).compile.drain
                      case "body-cancel" =>
                        for {
                          fiber <- response.body.compile.drain.start
                          _ <- entered.get
                          _ <- fiber.cancel
                        } yield ()
                      case _ => response.body.compile.drain
                    }) >> IO.sleep(7.seconds) >>
                    (if (mode == "consumer-error-after-body") IO.raiseError[Unit](boom)
                     else IO.unit)
                }
              }
            _ <-
              if (mode == "app-cancel")
                for {
                  fiber <- run.start
                  _ <- entered.get
                  _ <- fiber.cancel
                } yield ()
              else
                run.attempt.map { result =>
                  val fails = (sourceError && mode != "caught-body-error") ||
                    mode.startsWith("consumer-error")
                  assertEquals(result, if (fails) Left(boom) else Right(()))
                }
            spans <- kit.finishedSpans
            _ <-
              if (unstarted)
                IO(assertEquals(SpanExpectations.find(spans, SpanExpectation.server("GET")), None))
              else assertSpan(spans, http, status, mode, sourceError, boom)
            context <- http.get.map(_.get)
            recorded <- kit.collectMetrics
          } yield {
            val active = MetricExpectation
              .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
              .exactlyPoints(
                PointExpectation.numeric(if (unstarted) 1L else 0L).attributesExact(start)
              )
            val terminal = start ++
              Option.unless(appFailure)(
                HttpAttributes.HttpResponseStatusCode(status.code.toLong)
              ) ++
              Attributes(NetworkAttributes.NetworkProtocolVersion("1.1")) ++
              (if (sourceError) Attributes(ErrorAttributes.ErrorType(boom.getClass.getName))
               else if (errorResponse)
                 Attributes(ErrorAttributes.ErrorType("503"))
               else Attributes.empty)
            val seconds = if (appFailure) 2.0 else 10.0
            def histogram(name: String, value: Double): MetricExpectation = MetricExpectation
              .histogram(name)
              .exactlyPoints(
                PointExpectation.histogram
                  .count(1L)
                  .sum(value)
                  .attributesExact(terminal)
                  .exemplarCount(1)
                  .containsExemplars(
                    ExemplarExpectation
                      .numeric(value)
                      .traceContext(
                        TraceContext(context.traceId, context.spanId, context.isSampled)
                      )
                  )
              )
            MetricExpectations
              .checkAllDistinct(recorded, active)
              .fold(m => fail(MetricExpectations.format(m)), identity)
            if (unstarted)
              List(
                HttpMetrics.ServerRequestDuration.name,
                HttpExperimentalMetrics.ServerRequestBodySize.name,
                HttpExperimentalMetrics.ServerResponseBodySize.name,
              ).foreach { name =>
                assertEquals(
                  MetricExpectations.find(recorded, MetricExpectation.name(name)),
                  None,
                )
              }
            else {
              val sizes = List(histogram(HttpExperimentalMetrics.ServerRequestBodySize.name, 4d)) ++
                Option.unless(appFailure)(
                  histogram(HttpExperimentalMetrics.ServerResponseBodySize.name, 2d)
                )
              MetricExpectations
                .checkAllDistinct(
                  recorded,
                  (List(histogram(HttpMetrics.ServerRequestDuration.name, seconds)) ++ sizes): _*
                )
                .fold(m => fail(MetricExpectations.format(m)), identity)
              if (appFailure)
                assertEquals(
                  MetricExpectations
                    .find(
                      recorded,
                      MetricExpectation.name(HttpExperimentalMetrics.ServerResponseBodySize.name),
                    ),
                  None,
                )
            }
          }
        }
      }
    }
  }

  private def createMiddleware(
      kit: OpenTelemetrySdkTestkit[IO],
      metricsConfig: ServerMetricsConfig,
      tracingErrors: ErrorClassifier,
  ): IO[HttpApp[IO] => HttpApp[IO]] = {
    implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
    implicit val contextManager: ContextManager[IO] = kit.contextManager
    implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
    ServerInstrumentation.middleware[IO](
      metricsConfig,
      ServerTracingConfig(
        ServerSpanDataProvider.openTelemetry(
          new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {}
        )
      ).withErrorClassifier(tracingErrors),
    )
  }

  private def assertSpan(
      spans: List[org.typelevel.otel4s.sdk.trace.data.SpanData],
      http: Ref[IO, Option[SpanContext]],
      status: Status,
      mode: String,
      sourceError: Boolean,
      boom: Throwable,
  ): IO[Unit] = http.get.map { context =>
    val responseError = status == Status.ServiceUnavailable && mode != "trace-classifier-never"
    val failed = sourceError || responseError
    val expected = SpanExpectation
      .server("GET")
      .spanContextExact(context.get)
      .status(
        if (failed) StatusExpectation.error
        else StatusExpectation.unset
      )
      .eventCount(if (sourceError) 1 else 0)
      .where("span end time matches the recorded duration") { span =>
        span.endTimestamp.contains(
          span.startTimestamp + (if (mode.startsWith("app-")) 2.seconds else 10.seconds)
        )
      }
      .where("app and body errors override response status errors") { span =>
        span.attributes.elements.get(ErrorAttributes.ErrorType).map(_.value) ==
          (if (sourceError) Some(boom.getClass.getName)
           else Option.when(responseError)("503"))
      }
      .where("response status is available only after app success") { span =>
        span.attributes.elements.get(HttpAttributes.HttpResponseStatusCode).map(_.value) ==
          Option.unless(mode.startsWith("app-"))(status.code.toLong)
      }
    SpanExpectations
      .checkAllDistinct(
        spans,
        if (sourceError)
          expected.containsEvents(
            EventExpectation
              .name("exception")
              .attributesSubset(ExceptionAttributes.ExceptionType(boom.getClass.getName))
          )
        else expected,
      )
      .fold(m => fail(SpanExpectations.format(m)), identity)
  }
}
