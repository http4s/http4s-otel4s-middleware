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

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Headers
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.syntax.literals._
import org.typelevel.otel4s.trace.TracerProvider
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.context.propagation.TextMapGetter
import org.typelevel.otel4s.context.propagation.TextMapPropagator
import org.typelevel.otel4s.context.propagation.TextMapUpdater
import org.typelevel.otel4s.sdk.context.Context
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
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CBaggagePropagator
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.ExceptionAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext

import scala.concurrent.duration._

/** Public factory lifecycle and context coverage. */
class ClientInstrumentationLifecycleTest extends CatsEffectSuite {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val start = Attributes(
    HttpAttributes.HttpRequestMethod("GET"),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )
  private val terminal = start ++ Attributes(
    HttpAttributes.HttpResponseStatusCode(200L),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

  private case class Scenario(
      name: String,
      metricSeconds: Double,
      metricError: Boolean = false,
      callerError: Boolean = false,
  )

  private val scenarios = List(
    Scenario("full", 10),
    Scenario("partial", 10),
    Scenario("unread", 9),
    Scenario("body-error", 10, true, true),
    Scenario("caught-body-error", 10, true),
    Scenario("body-cleanup-error", 10, true, true),
    Scenario("body-cancel", 10),
    Scenario("consumer-error-before-body", 9, callerError = true),
    Scenario("consumer-error-after-body", 10, callerError = true),
    Scenario("backend-release-error", 10, callerError = true),
    Scenario("acquisition-error", 2, true, true),
    Scenario("acquisition-cancel", 2),
  )

  scenarios.foreach { scenario =>
    test(s"body-aware tracing matches metrics: ${scenario.name}") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          for {
            tracer <- kit.tracerProvider.get("test")
            http <- Ref.of[IO, Option[SpanContext]](None)
            atRelease <- Ref.of[IO, List[SpanData]](Nil)
            tracing <- createMiddleware(kit)
            entered <- Deferred[IO, Unit]
            boom = new IllegalStateException("failure")
            acquisitionFailure = scenario.name.startsWith("acquisition-")
            body = (Stream.sleep_[IO](3.seconds) ++
              (if (scenario.name == "body-cancel")
                 Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
               else Stream.emits(Vector[Byte](1, 2)).covary[IO]) ++
              (if (Set("body-error", "caught-body-error")(scenario.name))
                 Stream.raiseError[IO](boom)
               else Stream.empty))
              .onFinalize(
                IO.sleep(5.seconds) >>
                  (if (scenario.name == "body-cleanup-error") IO.raiseError[Unit](boom)
                   else IO.unit)
              )
            base = Client[IO](_ =>
              Resource.makeFull[IO, Response[IO]] { poll =>
                poll(
                  tracer.currentSpanContext.flatMap(http.set) >> IO.sleep(2.seconds) >>
                    (scenario.name match {
                      case "acquisition-error" => IO.raiseError[Response[IO]](boom)
                      case "acquisition-cancel" =>
                        entered.complete(()).void >> IO.never[Response[IO]]
                      case _ => IO.pure(Response[IO]().withBodyStream(body))
                    })
                )
              } { _ =>
                for {
                  spans <- kit.finishedSpans
                  _ <- atRelease.set(spans)
                  _ = SpanExpectations
                    .checkAllDistinct(spans, SpanExpectation.client("GET"))
                    .fold(m => fail(SpanExpectations.format(m)), identity)
                  _ <- IO.sleep(11.seconds)
                  _ <-
                    if (scenario.name == "backend-release-error") IO.raiseError[Unit](boom)
                    else IO.unit
                } yield ()
              }
            )
            run = tracing(base).run(request).use { response =>
              val consume = scenario.name match {
                case "unread" | "consumer-error-before-body" => IO.unit
                case "partial" => response.body.take(1).compile.drain
                case "caught-body-error" =>
                  response.body.handleErrorWith(_ => Stream.empty).compile.drain
                case "body-cancel" =>
                  for {
                    fiber <- response.body.compile.drain.start
                    _ <- entered.get
                    _ <- fiber.cancel
                    _ <- IO.canceled
                  } yield ()
                case _ => response.body.compile.drain
              }
              consume >> IO.sleep(7.seconds) >>
                (if (scenario.name.startsWith("consumer-error")) IO.raiseError[Unit](boom)
                 else IO.unit)
            }
            parent <- tracer.span("parent").use { parent =>
              val execute =
                if (scenario.name == "acquisition-cancel")
                  for {
                    fiber <- run.start
                    _ <- entered.get
                    _ <- fiber.cancel
                  } yield ()
                else if (scenario.name == "body-cancel") run.start.flatMap(_.join).void
                else
                  run.attempt.map { result =>
                    val fails = scenario.callerError
                    assertEquals(result, if (fails) Left(boom) else Right(()))
                  }
              execute.as(parent.context)
            }
            context <- http.get.map(_.get)
            recorded <- kit.collectMetrics
            endedAtRelease <- atRelease.get
            endedAfterRelease <- kit.finishedSpans
            spans = endedAtRelease ++ endedAfterRelease
          } yield {
            assertNotEquals(context.spanId, parent.spanId)
            val attributes = (if (acquisitionFailure) start else terminal) ++
              Option.when(scenario.metricError)(ErrorAttributes.ErrorType(boom.getClass.getName))
            MetricExpectations
              .checkAllDistinct(
                recorded,
                MetricExpectation
                  .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
                  .exactlyPoints(PointExpectation.numeric(0L).attributesExact(start)),
                MetricExpectation
                  .histogram(HttpMetrics.ClientRequestDuration.name)
                  .exactlyPoints(
                    PointExpectation.histogram
                      .count(1L)
                      .sum(scenario.metricSeconds)
                      .attributesExact(attributes)
                      .exemplarCount(1)
                      .containsExemplars(
                        ExemplarExpectation
                          .numeric(scenario.metricSeconds)
                          .traceContext(
                            TraceContext(context.traceId, context.spanId, context.isSampled)
                          )
                      )
                  ),
              )
              .fold(m => fail(MetricExpectations.format(m)), identity)
            val expected = SpanExpectation
              .client("GET")
              .spanContextExact(context)
              .parentSpanContextExact(parent)
              .status(
                if (scenario.metricError) StatusExpectation.error else StatusExpectation.unset
              )
              .eventCount(if (scenario.metricError) 1 else 0)
              .where("span duration matches the duration metric") { span =>
                span.endTimestamp.contains(span.startTimestamp + scenario.metricSeconds.seconds)
              }
              .where("response status survives source failures") { span =>
                span.attributes.elements.get(HttpAttributes.HttpResponseStatusCode).map(_.value) ==
                  Option.unless(acquisitionFailure)(200L)
              }
              .where("only observed acquisition/body errors set exception error.type") { span =>
                span.attributes.elements.get(ErrorAttributes.ErrorType).map(_.value) ==
                  Option.when(scenario.metricError)(boom.getClass.getName)
              }
            SpanExpectations
              .checkAllDistinct(
                spans,
                if (scenario.metricError)
                  expected.containsEvents(
                    EventExpectation
                      .name("exception")
                      .attributesSubset(
                        ExceptionAttributes.ExceptionType(boom.getClass.getName)
                      )
                  )
                else expected,
              )
              .fold(m => fail(SpanExpectations.format(m)), identity)
          }
        }
      }
    }
  }

  List(false, true).foreach { detached =>
    test(s"body effects and children retain the full HTTP context: detached=$detached") {
      Context.Key.unique[IO, String]("custom propagated value").flatMap { key =>
        val custom = new TextMapPropagator[Context] {
          val fields: List[String] = List("x-custom")
          def extract[A: TextMapGetter](ctx: Context, carrier: A): Context =
            TextMapGetter[A].get(carrier, "x-custom").fold(ctx)(ctx.updated(key, _))
          def inject[A: TextMapUpdater](ctx: Context, carrier: A): A =
            ctx.get(key).fold(carrier)(TextMapUpdater[A].updated(carrier, "x-custom", _))
        }
        OpenTelemetrySdkTestkit
          .inMemory[IO](
            _.addTextMapPropagators(W3CBaggagePropagator.default, custom)
          )
          .use { kit =>
            def headers(value: String): Headers =
              Headers("baggage" -> s"tenant=$value", "x-custom" -> value)
            for {
              tracer <- kit.tracerProvider.get("test")
              tracing <- createMiddleware(kit)
              http <- Ref.of[IO, Option[SpanContext]](None)
              phases <- Ref.of[IO, Vector[String]](Vector.empty)
              _ <- tracer.joinOrRoot(headers("request")) {
                val expectedValue = Some("request")
                def observe(phase: String): IO[Unit] = for {
                  baggage <- kit.baggageManager.getValue("tenant")
                  context <- kit.localContext.ask[Context]
                  _ = assertEquals(baggage, expectedValue)
                  _ = assertEquals(context.get(key), expectedValue)
                  expected <- http.get
                  current <- tracer.currentSpanContext
                  _ = assertEquals(current, expected)
                  _ <- phases.update(_ :+ phase)
                } yield ()
                val body = Stream
                  .exec(observe("body") >> tracer.span("body-child").use(_ => IO.unit))
                  .append(Stream.emits(Vector[Byte](1, 2)).covary[IO])
                  .onFinalize(observe("cleanup") >> tracer.span("cleanup-child").use(_ => IO.unit))
                val base = Client[IO](req =>
                  Resource.eval(for {
                    current <- tracer.currentSpanContext
                    _ <- http.set(current)
                    _ <- req.body.compile.drain
                    baggage <- kit.baggageManager.getValue("tenant")
                    context <- kit.localContext.ask[Context]
                    _ = assertEquals(baggage, Some("request"))
                    _ = assertEquals(context.get(key), Some("request"))
                  } yield Response[IO]().withBodyStream(body))
                )
                val outgoing = request.withBodyStream(Stream.exec(observe("request-body")))
                tracing(base).run(outgoing).use { response =>
                  def consume(expectedSpan: Option[SpanContext], expectedValue: Option[String])
                      : IO[Unit] =
                    response.body
                      .evalMap { byte =>
                        for {
                          current <- tracer.currentSpanContext
                          baggage <- kit.baggageManager.getValue("tenant")
                          context <- kit.localContext.ask[Context]
                          _ = assertEquals(current, expectedSpan)
                          _ = assertEquals(baggage, expectedValue)
                          _ = assertEquals(context.get(key), expectedValue)
                        } yield byte
                      }
                      .compile
                      .drain
                  if (detached)
                    kit.localContext.scope(consume(None, None))(Context.root).as(None)
                  else
                    tracer.joinOrRoot(headers("consumer")) {
                      tracer.span("consumer").use { span =>
                        consume(Some(span.context), Some("consumer")) >> (
                          for {
                            baggage <- kit.baggageManager.getValue("tenant")
                            context <- kit.localContext.ask[Context]
                            _ = assertEquals(baggage, Some("consumer"))
                            _ = assertEquals(context.get(key), Some("consumer"))
                            _ <- phases.update(_ :+ "restored")
                          } yield ()
                        ) >>
                          tracer.currentSpanContext.map { current =>
                            assertEquals(current, Some(span.context))
                            Some(span.context)
                          }
                      }
                    }
                }
              }
              context <- http.get.map(_.get)
              observed <- phases.get
              spans <- kit.finishedSpans
            } yield {
              assertEquals(
                observed,
                if (detached) Vector("request-body", "body", "cleanup")
                else Vector("request-body", "body", "cleanup", "restored"),
              )
              val child = SpanExpectation.name("body-child")
              SpanExpectations
                .checkAllDistinct(
                  spans,
                  SpanExpectation.client("GET").spanContextExact(context),
                  child.parentSpanContextExact(context),
                  SpanExpectation.name("cleanup-child").parentSpanContextExact(context),
                )
                .fold(m => fail(SpanExpectations.format(m)), identity)
            }
          }
      }
    }
  }
  private def createMiddleware(kit: OpenTelemetrySdkTestkit[IO]): IO[Client[IO] => Client[IO]] = {
    implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
    implicit val contextManager: ContextManager[IO] = kit.contextManager
    implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
    ClientInstrumentation.middleware[IO](
      ClientMetricsConfig.default.optIntoActiveRequests,
      ClientTracingConfig(
        ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
      ),
    )
  }

}
