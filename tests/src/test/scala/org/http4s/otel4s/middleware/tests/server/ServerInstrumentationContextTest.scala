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
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
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
import org.typelevel.otel4s.sdk.testkit.trace.SpanContextExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CBaggagePropagator
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext

import scala.concurrent.duration._

class ServerInstrumentationContextTest extends CatsEffectSuite {
  private val remoteTrace = "0123456789abcdef0123456789abcdef"
  private val remoteSpan = "0123456789abcdef"
  List(false, true).foreach { detached =>
    test(s"streaming preserves full producer context and consumer scope: detached=$detached") {
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
                http <- Ref.of[IO, Option[SpanContext]](None)
                phases <- Ref.of[IO, Vector[String]](Vector.empty)
                next <- Deferred[IO, Unit]
                observe = (phase: String) =>
                  for {
                    expected <- http.get
                    current <- tracer.currentSpanContext
                    baggage <- kit.baggageManager.getValue("tenant")
                    context <- kit.localContext.ask[Context]
                    _ = assertEquals(current, expected)
                    _ = assertEquals(baggage, Some("request"))
                    _ = assertEquals(context.get(key), Some("request"))
                    _ <- phases.update(_ :+ phase)
                  } yield ()
                middleware <- createMiddleware(
                  kit,
                  metricsConfig = ServerMetricsConfig.default.optIntoActiveRequests
                    .optIntoResponseBodySize(BodySizeConfig.observe),
                )
                body = (Stream.exec(
                  observe("body") >> tracer.span("body-child").use(_ => IO.unit)
                ) ++
                  Stream.emit(1.toByte).covary[IO] ++
                  Stream.exec(next.get >> observe("continued") >> IO.sleep(3.seconds)) ++
                  Stream.emits(Vector[Byte](2, 3)).covary[IO])
                  .onFinalize(observe("cleanup") >> IO.sleep(5.seconds))
                app = middleware(
                  HttpApp[IO](req =>
                    tracer.currentSpanContext.flatMap(http.set) >> observe("app") >> IO
                      .sleep(2.seconds)
                      .as(
                        Response[IO]().withBodyStream(Stream.exec(req.body.compile.drain) ++ body)
                      )
                  )
                )
                _ <- tracer.joinOrRoot(headers("caller")) {
                  tracer.span("caller").use { caller =>
                    for {
                      response <- app(
                        Request[IO](
                          uri = uri"https://example.test/items",
                          headers = headers("request"),
                        ).withBodyStream(
                          Stream.exec(
                            observe("request") >> tracer.span("request-child").use(_ => IO.unit)
                          )
                        )
                      )
                      consume = (expected: Option[SpanContext], value: Option[String]) =>
                        response.body
                          .evalMap { byte =>
                            for {
                              current <- tracer.currentSpanContext
                              baggage <- kit.baggageManager.getValue("tenant")
                              ctx <- kit.localContext.ask[Context]
                              _ = assertEquals(current, expected)
                              _ = assertEquals(baggage, value)
                              _ = assertEquals(ctx.get(key), value)
                              // The tail cannot progress until the consumer receives the first byte.
                              _ <- next.complete(()).void.whenA(byte == 1.toByte)
                            } yield byte
                          }
                          .compile
                          .toVector
                          .flatMap(bytes => IO(assertEquals(bytes, Vector[Byte](1, 2, 3)))) >>
                          tracer.currentSpanContext.map(current => assertEquals(current, expected))
                      _ <-
                        if (detached) kit.localContext.scope(consume(None, None))(Context.root)
                        else
                          tracer.joinOrRoot(headers("consumer")) {
                            tracer
                              .span("consumer")
                              .use(span => consume(Some(span.context), Some("consumer")))
                          }
                      current <- tracer.currentSpanContext
                      baggage <- kit.baggageManager.getValue("tenant")
                      context <- kit.localContext.ask[Context]
                      _ = assertEquals(current, Some(caller.context))
                      _ = assertEquals(baggage, Some("caller"))
                      _ = assertEquals(context.get(key), Some("caller"))
                    } yield ()
                  }
                }
                context <- http.get.map(_.get)
                metrics <- kit.collectMetrics
                after <- kit.finishedSpans
                seen <- phases.get
              } yield {
                assertEquals(
                  seen,
                  Vector("app", "request", "body", "continued", "cleanup"),
                )
                val labels = Attributes(
                  HttpAttributes.HttpRequestMethod("GET"),
                  UrlAttributes.UrlScheme("https"),
                )
                val terminal = labels ++ Attributes(
                  HttpAttributes.HttpResponseStatusCode(200L),
                  NetworkAttributes.NetworkProtocolVersion("1.1"),
                )
                def histogram(name: String, value: Double): MetricExpectation =
                  MetricExpectation
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
                  .checkAllDistinct(
                    metrics,
                    MetricExpectation
                      .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                      .exactlyPoints(PointExpectation.numeric(0L).attributesExact(labels)),
                    histogram(HttpMetrics.ServerRequestDuration.name, 10d),
                    histogram(HttpExperimentalMetrics.ServerResponseBodySize.name, 3d),
                  )
                  .fold(m => fail(MetricExpectations.format(m)), identity)
                SpanExpectations
                  .checkAllDistinct(
                    after,
                    SpanExpectation
                      .server("GET")
                      .spanContextExact(context)
                      .parentSpanContext(
                        SpanContextExpectation.any
                          .traceIdHex(remoteTrace)
                          .spanIdHex(remoteSpan)
                          .remote(true)
                      )
                      .where("body lifetime includes cleanup") { span =>
                        span.endTimestamp.contains(span.startTimestamp + 10.seconds)
                      },
                    SpanExpectation.name("body-child").parentSpanContextExact(context),
                    SpanExpectation.name("request-child").parentSpanContextExact(context),
                    SpanExpectation.name("caller"),
                  )
                  .fold(m => fail(SpanExpectations.format(m)), identity)
              }
            }
        }
      }
    }
  }
  private def createMiddleware(
      kit: OpenTelemetrySdkTestkit[IO],
      metricsConfig: ServerMetricsConfig,
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
      ),
    )
  }

  private def headers(value: String): Headers = Headers(
    "traceparent" -> s"00-$remoteTrace-$remoteSpan-01",
    "baggage" -> s"tenant=$value",
    "x-custom" -> value,
  )
  private def propagator(key: Context.Key[String]): TextMapPropagator[Context] =
    new TextMapPropagator[Context] {
      val fields: List[String] = List("x-custom")
      def extract[A: TextMapGetter](ctx: Context, carrier: A): Context =
        TextMapGetter[A].get(carrier, "x-custom").fold(ctx)(ctx.updated(key, _))
      def inject[A: TextMapUpdater](ctx: Context, carrier: A): A =
        ctx.get(key).fold(carrier)(TextMapUpdater[A].updated(carrier, "x-custom", _))
    }

}
