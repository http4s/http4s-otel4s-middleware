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
import cats.syntax.all._
import fs2.Stream
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.context.propagation.TextMapGetter
import org.typelevel.otel4s.context.propagation.TextMapPropagator
import org.typelevel.otel4s.context.propagation.TextMapUpdater
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.ExemplarExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanContextExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CBaggagePropagator
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration._

class ServerMetricsContextTest extends CatsEffectSuite {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val terminal = start ++ Attributes(
    HttpAttributes.HttpResponseStatusCode(200L),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )
  List("success", "detached", "error", "caught-error", "cancel", "partial", "replay").foreach {
    mode =>
      test(s"body effects, cleanup, children and exemplars retain the full HTTP context: $mode") {
        TestControl.executeEmbed {
          Context.Key.unique[IO, String]("custom propagated value").flatMap { key =>
            OpenTelemetrySdkTestkit
              .inMemory[IO](
                _.addTextMapPropagators(
                  W3CTraceContextPropagator.default,
                  W3CBaggagePropagator.default,
                  propagator(key),
                )
              )
              .use { kit =>
                for {
                  tracer <- kit.tracerProvider.get("test")
                  middleware <- ServerMetrics.middleware[IO](
                    kit,
                    ServerMetricsConfig.default.optIntoActiveRequests,
                  )
                  entered <- Deferred[IO, Unit]
                  seen <- Ref.of[IO, Vector[String]](Vector.empty)
                  boom = new IllegalStateException("body")
                  contexts <- tracer.span("parent").use { parent =>
                    tracer.propagate(Headers.empty).flatMap { traceHeaders =>
                      val propagated = traceHeaders.put(
                        Header.Raw(ci"baggage", "tenant=request"),
                        Header.Raw(ci"x-custom", "request"),
                      )
                      tracer.joinOrRoot(propagated) {
                        tracer.spanBuilder("HTTP").withSpanKind(SpanKind.Server).build.use { http =>
                          def observe(phase: String): IO[Unit] = for {
                            current <- tracer.currentSpanContext
                            baggage <- kit.baggageManager.getValue("tenant")
                            ctx <- kit.localContext.ask[Context]
                            _ = assertEquals(current, Some(http.context))
                            _ = assertEquals(baggage, Some("request"))
                            _ = assertEquals(ctx.get(key), Some("request"))
                            _ <- seen.update(_ :+ phase)
                          } yield ()
                          val body = (Stream.sleep_[IO](2.seconds) ++ Stream.exec(
                            observe("body") >>
                              tracer.span("body-child").use(_ => IO.unit)
                          ) ++
                            (if (mode == "cancel")
                               Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
                             else
                               Stream.emits(Vector[Byte](1, 2, 3)).covary[IO] ++
                                 (if (mode.contains("error")) Stream.raiseError[IO](boom)
                                  else Stream.empty)))
                            .onFinalize(observe("cleanup") >> IO.sleep(3.seconds))
                          val app = middleware(
                            HttpApp[IO](_ =>
                              observe("app") >> IO
                                .sleep(1.second)
                                .as(Response[IO]().withBodyStream(body))
                            )
                          )
                          for {
                            response <- app(request)
                            consume = mode match {
                              case "cancel" =>
                                for {
                                  fiber <- response.body.compile.drain.start
                                  _ <- entered.get
                                  _ <- fiber.cancel
                                } yield ()
                              case "error" =>
                                response.body.compile.drain.attempt
                                  .flatMap(r => IO(assertEquals(r, Left(boom))))
                              case "caught-error" =>
                                response.body.handleErrorWith(_ => Stream.empty).compile.drain
                              case "partial" => response.body.take(1).compile.drain
                              case "replay" =>
                                response.body.compile.drain >> response.body.compile.drain
                              case _ => response.body.compile.drain
                            }
                            _ <-
                              if (mode == "detached") kit.localContext.scope(consume)(Context.root)
                              else
                                tracer.joinOrRoot(headers("consumer")) {
                                  tracer.span("consumer").use { consumer =>
                                    consume >> (
                                      tracer.currentSpanContext,
                                      kit.baggageManager.getValue("tenant"),
                                      kit.localContext.ask[Context],
                                    ).tupled.map { case (current, baggage, ctx) =>
                                      assertEquals(current, Some(consumer.context))
                                      assertEquals(baggage, Some("consumer"))
                                      assertEquals(ctx.get(key), Some("consumer"))
                                    }
                                  }
                                }
                            metrics <- kit.collectMetrics
                            open <- http.isRecording
                            _ = assert(open, "metrics must be recorded before the HTTP span ends")
                            spans <- kit.finishedSpans
                            _ = assertEquals(
                              SpanExpectations.find(spans, SpanExpectation.name("HTTP")),
                              None,
                            )
                            _ = SpanExpectations
                              .checkAllDistinct(
                                spans,
                                SpanExpectation
                                  .name("body-child")
                                  .parentSpanContextExact(http.context),
                              )
                              .fold(m => fail(SpanExpectations.format(m)), identity)
                            _ = assertMetrics(
                              metrics,
                              http.context,
                              6d,
                              terminal ++
                                Option.when(mode.contains("error"))(
                                  ErrorAttributes.ErrorType(boom.getClass.getName)
                                ),
                            )
                            current <- tracer.currentSpanContext
                            _ = assertEquals(current, Some(http.context))
                          } yield (parent.context, http.context)
                        }
                      }
                    }
                  }
                  observations <- seen.get
                  spans <- kit.finishedSpans
                  ambient <- tracer.currentSpanContext
                  baggage <- kit.baggageManager.current
                  ctx <- kit.localContext.ask[Context]
                } yield {
                  val (parent, http) = contexts
                  assertNotEquals(parent.spanId, http.spanId)
                  assertEquals(observations.count(_ == "app"), 1)
                  assertEquals(observations.count(_ == "body"), if (mode == "replay") 2 else 1)
                  assertEquals(observations.count(_ == "cleanup"), if (mode == "replay") 2 else 1)
                  assertEquals(ambient, None)
                  assert(baggage.isEmpty)
                  assertEquals(ctx.get(key), None)
                  SpanExpectations
                    .checkAllDistinct(
                      spans,
                      SpanExpectation.name("parent").spanContextExact(parent),
                      SpanExpectation
                        .server("HTTP")
                        .spanContextExact(http)
                        .parentSpanContext(SpanContextExpectation.exact(parent).remote(true)),
                    )
                    .fold(m => fail(SpanExpectations.format(m)), identity)
                }
              }
          }
        }
      }
  }

  List(true, false).foreach { tracingOutside =>
    test(
      s"existing tracing ends at app return; exemplar reflects wrapping order: tracingOutside=$tracingOutside"
    ) {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit
          .inMemory[IO](_.addTextMapPropagators(W3CTraceContextPropagator.default))
          .use { kit =>
            implicit val provider: TracerProvider[IO] = kit.tracerProvider
            for {
              tracer <- kit.tracerProvider.get("test")
              middleware <- ServerMetrics
                .middleware[IO](kit, ServerMetricsConfig.default.optIntoActiveRequests)
              tracing <- ServerMiddleware
                .builder[IO](
                  ServerSpanDataProvider
                    .openTelemetry(new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {})
                )
                .build
              httpRef <- Ref.of[IO, Option[SpanContext]](None)
              base = HttpApp[IO](_ =>
                tracer.currentSpanContext.flatMap(httpRef.set) >> IO
                  .sleep(1.second)
                  .as(Response[IO]().withBodyStream(Stream.sleep_[IO](2.seconds)))
              )
              app =
                if (tracingOutside) tracing.wrapHttpApp(middleware(base))
                else middleware(tracing.wrapHttpApp(base))
              contexts <- tracer.span("parent").use { parent =>
                // The public tracing wrapper extracts headers; explicitly propagate the parent.
                tracer.propagate(Headers.empty).flatMap { propagated =>
                  for {
                    response <- app(request.withHeaders(propagated))
                    http <- httpRef.get.map(_.get)
                    ended <- kit.finishedSpans
                    _ = SpanExpectations
                      .checkAllDistinct(
                        ended,
                        SpanExpectation
                          .server("GET")
                          .spanContextExact(http)
                          .parentSpanContext(
                            SpanContextExpectation.exact(parent.context).remote(true)
                          ),
                      )
                      .fold(m => fail(SpanExpectations.format(m)), identity)
                    _ <- tracer.span("consumer").use(_ => response.body.compile.drain)
                    metrics <- kit.collectMetrics
                    _ = assertMetrics(
                      metrics,
                      if (tracingOutside) http else parent.context,
                      3d,
                      terminal,
                    )
                  } yield (parent.context, http)
                }
              }
              spans <- kit.finishedSpans
            } yield {
              val (parent, http) = contexts
              assertNotEquals(parent.spanId, http.spanId)
              SpanExpectations
                .checkAllDistinct(
                  spans,
                  SpanExpectation.name("parent").spanContextExact(parent),
                )
                .fold(m => fail(SpanExpectations.format(m)), identity)
            }
          }
      }
    }
  }

  private def headers(value: String): Headers =
    Headers("baggage" -> s"tenant=$value", "x-custom" -> value)

  private def propagator(key: Context.Key[String]): TextMapPropagator[Context] =
    new TextMapPropagator[Context] {
      val fields: List[String] = List("x-custom")
      def extract[A: TextMapGetter](ctx: Context, carrier: A): Context =
        TextMapGetter[A].get(carrier, "x-custom").fold(ctx)(ctx.updated(key, _))
      def inject[A: TextMapUpdater](ctx: Context, carrier: A): A =
        ctx.get(key).fold(carrier)(TextMapUpdater[A].updated(carrier, "x-custom", _))
    }

  private def assertMetrics(
      metrics: List[MetricData],
      context: SpanContext,
      seconds: Double,
      attributes: Attributes,
  )(implicit loc: Location): Unit =
    MetricExpectations
      .checkAllDistinct(
        metrics,
        MetricExpectation
          .sum[Long]("http.server.active_requests")
          .exactlyPoints(PointExpectation.numeric(0L).attributesExact(start)),
        MetricExpectation
          .histogram("http.server.request.duration")
          .exactlyPoints(
            PointExpectation.histogram
              .count(1L)
              .sum(seconds)
              .attributesExact(attributes)
              .exemplarCount(1)
              .containsExemplars(
                ExemplarExpectation
                  .numeric(seconds)
                  .traceContext(TraceContext(context.traceId, context.spanId, context.isSampled))
              )
          ),
      )
      .fold(m => fail(MetricExpectations.format(m)), identity)

}
