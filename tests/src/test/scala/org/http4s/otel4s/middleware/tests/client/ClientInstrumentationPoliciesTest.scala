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
import cats.syntax.all._
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Headers
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.SpanDataProvider
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.metrics.view.InstrumentSelector
import org.typelevel.otel4s.sdk.metrics.view.View
import org.typelevel.otel4s.sdk.testkit.InstrumentationScopeExpectation
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
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.sdk.trace.samplers.Sampler
import org.typelevel.otel4s.semconv.SchemaUrls
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.TracerProvider

class ClientInstrumentationPoliciesTest extends CatsEffectSuite {
  private val request = Request[IO](uri = uri"https://example.test/items")
  private val provider = ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
  private val excludedProvider = new SpanDataProvider {
    type Shared = Unit
    def processSharedData[F[_]](request: Request[F]): Unit =
      throw new AssertionError("excluded tracing must not process request data")
    def spanName[F[_]](request: Request[F], shared: Unit): String = "excluded"
    def requestAttributes[F[_]](request: Request[F], shared: Unit): Attributes = Attributes.empty
    def responseAttributes[F[_]](response: Response[F]): Attributes = Attributes.empty
    def exceptionAttributes(cause: Throwable): Attributes = Attributes.empty
  }

  List(
    (false, false, false),
    (false, true, false),
    (true, false, false),
    (true, false, true),
    (true, true, false),
    (true, true, true),
  ).foreach { case (trace, meter, propagate) =>
    test(
      s"public independent policies and producer/consumer contexts: trace=$trace, meter=$meter, propagate=$propagate"
    ) {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit
          .inMemory[IO](_.addTextMapPropagators(W3CTraceContextPropagator.default))
          .use { kit =>
            implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
            implicit val contextManager: ContextManager[IO] = kit.contextManager
            implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
            for {
              tracer <- kit.tracerProvider.get("test")
              received <- Ref.of[IO, Headers](Headers.empty)
              http <- Ref.of[IO, Option[SpanContext]](None)
              producer <- Ref.of[IO, Option[SpanContext]](None)
              cleanup <- Ref.of[IO, Option[SpanContext]](None)
              wrap <- ClientInstrumentation.middleware[IO](
                ClientMetricsConfig.default.optIntoActiveRequests
                  .withRequestFilter(filter(meter))
                  .withRequestAttributes { _ =>
                    assert(meter, "excluded metrics must not extract labels")
                    Attributes.empty
                  },
                ClientTracingConfig(excludedProvider)
                  .withSpanDataProvider(if (trace) provider else excludedProvider)
                  .withRequestFilter(filter(trace))
                  .withPropagationFilter(filter(propagate)),
              )
              body = (Stream.exec(tracer.currentSpanContext.flatMap(producer.set)) ++ Stream
                .emit[IO, Byte](1))
                .onFinalize(tracer.currentSpanContext.flatMap(cleanup.set))
              base = Client[IO](req =>
                Resource
                  .eval(
                    received.set(req.headers) >> tracer.currentSpanContext.flatMap(http.set)
                  )
                  .as(Response[IO]().withBodyStream(body))
              )
              contexts <- tracer.span("parent").use { parent =>
                tracer.propagate(Headers("x-existing" -> "keep")).flatMap { parentHeaders =>
                  wrap(base).run(request.withHeaders(parentHeaders)).use { response =>
                    tracer.span("consumer").use { consumer =>
                      response.body
                        .evalMap { byte =>
                          tracer.currentSpanContext.map { current =>
                            assertEquals(current, Some(consumer.context))
                            byte
                          }
                        }
                        .compile
                        .drain
                        .as((parent.context, consumer.context, parentHeaders))
                    }
                  }
                }
              }
              (parent, consumer, originalHeaders) = contexts
              context <- http.get.map(_.get)
              sourceContext <- producer.get
              cleanupContext <- cleanup.get
              headers <- received.get
              metrics <- kit.collectMetrics
              spans <- kit.finishedSpans
            } yield {
              val expectedSource = if (trace) context else consumer
              assertEquals(sourceContext, Some(expectedSource))
              assertEquals(cleanupContext, Some(expectedSource))
              assertEquals(headers.get(ci"x-existing"), originalHeaders.get(ci"x-existing"))
              if (trace && propagate)
                assertEquals(
                  headers.get(ci"traceparent").map(_.head.value),
                  Some(
                    s"00-${context.traceIdHex}-${context.spanIdHex}-${context.traceFlags.toHex}"
                  ),
                )
              else
                assertEquals(headers.get(ci"traceparent"), originalHeaders.get(ci"traceparent"))
              if (meter)
                MetricExpectations
                  .checkAllDistinct(
                    metrics,
                    MetricExpectation
                      .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
                      .exactlyPoints(PointExpectation.numeric(0L)),
                    MetricExpectation
                      .histogram(HttpMetrics.ClientRequestDuration.name)
                      .exactlyPoints(
                        PointExpectation.histogram
                          .count(1L)
                          .sum(0d)
                          .exemplarCount(1)
                          .containsExemplars(
                            ExemplarExpectation
                              .numeric(0d)
                              .traceContext(
                                TraceContext(context.traceId, context.spanId, context.isSampled)
                              )
                          )
                      ),
                  )
                  .fold(m => fail(MetricExpectations.format(m)), identity)
              else {
                assertEquals(
                  MetricExpectations.find(
                    metrics,
                    MetricExpectation.name(HttpExperimentalMetrics.ClientActiveRequests.name),
                  ),
                  None,
                )
                assertEquals(
                  MetricExpectations
                    .find(metrics, MetricExpectation.name(HttpMetrics.ClientRequestDuration.name)),
                  None,
                )
              }
              val children = List(
                TraceExpectation
                  .leaf(SpanExpectation.name("consumer").spanContextExact(consumer))
              ) ++
                Option.when(trace)(
                  TraceExpectation.leaf(
                    SpanExpectation
                      .client("GET")
                      .spanContextExact(context)
                      .scope(
                        InstrumentationScopeExpectation
                          .name("org.http4s.otel4s.middleware.client")
                          .schemaUrl(SchemaUrls.V1_43_0)
                      )
                      .status(StatusExpectation.unset)
                      .eventCount(0)
                  )
                )
              TraceExpectations
                .check(
                  spans,
                  TraceForestExpectation.unordered(
                    TraceExpectation.unordered(
                      SpanExpectation.name("parent").spanContextExact(parent),
                      children: _*
                    )
                  ),
                )
                .fold(m => fail(TraceExpectations.format(m)), identity)
            }
          }
      }
    }
  }

  List(false, true).foreach { sourceError =>
    test(s"public tracing config keeps classification independent: sourceError=$sourceError") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
        implicit val contextManager: ContextManager[IO] = kit.contextManager
        implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
        val boom = new IllegalStateException("source")
        for {
          wrap <- ClientInstrumentation.middleware[IO](
            ClientMetricsConfig.default.optIntoActiveRequests,
            ClientTracingConfig(provider).withErrorClassifier(ErrorClassifier.never),
          )
          body = if (sourceError) Stream.raiseError[IO](boom) else Stream.empty
          base = Client[IO](_ =>
            Resource.pure(Response[IO](Status.ServiceUnavailable).withBodyStream(body))
          )
          _ <- wrap(base).run(request).use(_.body.handleErrorWith(_ => Stream.empty).compile.drain)
          metrics <- kit.collectMetrics
          spans <- kit.finishedSpans
        } yield {
          MetricExpectations
            .checkAllDistinct(
              metrics,
              MetricExpectation
                .histogram(HttpMetrics.ClientRequestDuration.name)
                .exactlyPoints(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesSubset(
                      HttpAttributes.HttpResponseStatusCode(503L),
                      ErrorAttributes.ErrorType(if (sourceError) boom.getClass.getName else "503"),
                    )
                ),
            )
            .fold(m => fail(MetricExpectations.format(m)), identity)
          TraceExpectations
            .check(
              spans,
              TraceForestExpectation.unordered(
                TraceExpectation.leaf(
                  SpanExpectation
                    .client("GET")
                    .status(if (sourceError) StatusExpectation.error else StatusExpectation.unset)
                    .eventCount(if (sourceError) 1 else 0)
                    .where("tracing applies its own response classifier") { span =>
                      span.attributes.elements.get(ErrorAttributes.ErrorType).map(_.value) == Option
                        .when(sourceError)(boom.getClass.getName)
                    }
                )
              ),
            )
            .fold(m => fail(TraceExpectations.format(m)), identity)
        }
      }
    }
  }

  test("unsampled HTTP context propagates but produces no trace-based exemplar") {
    OpenTelemetrySdkTestkit
      .inMemory[IO](
        _.addTextMapPropagators(W3CTraceContextPropagator.default)
          .addTracerProviderCustomizer(_.withSampler(Sampler.alwaysOff))
      )
      .use { kit =>
        implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
        implicit val contextManager: ContextManager[IO] = kit.contextManager
        implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
        for {
          tracer <- kit.tracerProvider.get("test")
          http <- Ref.of[IO, Option[SpanContext]](None)
          wrap <- ClientInstrumentation
            .middleware[IO](
              ClientMetricsConfig.default.optIntoActiveRequests,
              ClientTracingConfig(provider),
            )
          received <- Ref.of[IO, Headers](Headers.empty)
          base = Client[IO](req =>
            Resource
              .eval(received.set(req.headers) >> tracer.currentSpanContext.flatMap(http.set))
              .as(Response[IO]())
          )
          _ <- wrap(base).run(request).use(_.body.compile.drain)
          headers <- received.get
          context <- http.get.map(_.get)
          metrics <- kit.collectMetrics
          spans <- kit.finishedSpans
        } yield {
          assert(!context.isSampled)
          assertEquals(
            headers.get(ci"traceparent").map(_.head.value),
            Some(s"00-${context.traceIdHex}-${context.spanIdHex}-${context.traceFlags.toHex}"),
          )
          MetricExpectations
            .checkAllDistinct(
              metrics,
              MetricExpectation
                .histogram(HttpMetrics.ClientRequestDuration.name)
                .exactlyPoints(PointExpectation.histogram.count(1L).exemplarCount(0)),
              MetricExpectation
                .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
                .exactlyPoints(PointExpectation.numeric(0L)),
            )
            .fold(m => fail(MetricExpectations.format(m)), identity)
          TraceExpectations
            .check(spans, TraceForestExpectation.empty)
            .fold(m => fail(TraceExpectations.format(m)), identity)
        }
      }
  }

  List(
    "metrics initialization",
    "metrics response classification",
    "metric recording",
    "trace classification",
  ).foreach { failure =>
    test(s"$failure failure closes the span and preserves backend resource ownership") {
      val boom = new IllegalStateException("telemetry failure")
      OpenTelemetrySdkTestkit
        .inMemory[IO](
          _.addMeterProviderCustomizer(
            _.registerView(
              InstrumentSelector.builder
                .withInstrumentName(HttpMetrics.ClientRequestDuration.name)
                .build,
              View.builder
                .addAttributeFilter((_: Attribute[_]) =>
                  if (failure == "metric recording") throw boom else true
                )
                .build,
            )
          )
        )
        .use { kit =>
          implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
          implicit val contextManager: ContextManager[IO] = kit.contextManager
          implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
          val metricsConfig =
            ClientMetricsConfig.default.optIntoActiveRequests
              .withRequestAttributes { _ =>
                if (failure == "metrics initialization") throw boom else Attributes.empty
              }
              .withErrorClassifier { (_, _) =>
                if (failure == "metrics response classification") throw boom else false
              }
          val tracingConfig = ClientTracingConfig(provider).withErrorClassifier { (_, _) =>
            if (failure == "trace classification") throw boom else false
          }
          for {
            acquisitions <- Ref.of[IO, Int](0)
            releases <- Ref.of[IO, Int](0)
            wrap <- ClientInstrumentation.middleware[IO](metricsConfig, tracingConfig)
            base = Client[IO](_ =>
              Resource
                .make(acquisitions.update(_ + 1).as(Response[IO]()))(_ => releases.update(_ + 1))
            )
            result <- wrap(base).run(request).use(_ => IO.unit).attempt
            acquired <- acquisitions.get
            released <- releases.get
            metrics <- kit.collectMetrics
            spans <- kit.finishedSpans
          } yield {
            assertEquals(result, Left(boom))
            assertEquals(acquired, if (failure == "metrics initialization") 0 else 1)
            assertEquals(released, acquired)
            TraceExpectations
              .check(
                spans,
                TraceForestExpectation.unordered(
                  TraceExpectation.leaf(SpanExpectation.client("GET"))
                ),
              )
              .fold(mismatches => fail(TraceExpectations.format(mismatches)), identity)
            if (failure == "metrics initialization") {
              List(
                HttpMetrics.ClientRequestDuration.name,
                HttpExperimentalMetrics.ClientActiveRequests.name,
              ).foreach(name =>
                assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(name)), None)
              )
            } else {
              MetricExpectations
                .checkAllDistinct(
                  metrics,
                  MetricExpectation
                    .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
                    .exactlyPoints(PointExpectation.numeric(0L)),
                )
                .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
              if (failure == "trace classification")
                MetricExpectations
                  .checkAllDistinct(
                    metrics,
                    MetricExpectation
                      .histogram(HttpMetrics.ClientRequestDuration.name)
                      .exactlyPoints(PointExpectation.histogram.count(1L).exemplarCount(1)),
                  )
                  .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
            }
          }
        }
    }
  }

  test(
    "a reused resource creates independent exchanges and evaluates policies once per acquisition"
  ) {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
      implicit val contextManager: ContextManager[IO] = kit.contextManager
      implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
      var metricsFilters = 0
      var labels = 0
      var traceFilters = 0
      var propagationFilters = 0
      var processed = 0
      val countingProvider = new SpanDataProvider {
        type Shared = provider.Shared
        def processSharedData[F[_]](request: Request[F]): Shared = {
          processed += 1
          provider.processSharedData(request)
        }
        def spanName[F[_]](request: Request[F], shared: Shared): String =
          provider.spanName(request, shared)
        def requestAttributes[F[_]](request: Request[F], shared: Shared): Attributes =
          provider.requestAttributes(request, shared)
        def responseAttributes[F[_]](response: Response[F]): Attributes =
          provider.responseAttributes(response)
        def exceptionAttributes(cause: Throwable): Attributes = provider.exceptionAttributes(cause)
      }
      val metricsConfig = ClientMetricsConfig.default.optIntoActiveRequests
        .withRequestFilter { _ => metricsFilters += 1; PerRequestConfig.Enabled }
        .withRequestAttributes { _ => labels += 1; Attributes.empty }
      val tracingConfig = ClientTracingConfig(countingProvider)
        .withRequestFilter { _ => traceFilters += 1; PerRequestConfig.Enabled }
        .withPropagationFilter { _ => propagationFilters += 1; PerRequestConfig.Enabled }
      for {
        tracer <- kit.tracerProvider.get("test")
        contexts <- Ref.of[IO, Vector[SpanContext]](Vector.empty)
        releases <- Ref.of[IO, Int](0)
        wrap <- ClientInstrumentation.middleware[IO](metricsConfig, tracingConfig)
        base = Client[IO](_ =>
          Resource.make(
            tracer.currentSpanContext
              .flatMap(ctx => contexts.update(_ :+ ctx.get))
              .as(Response[IO]())
          )(_ => releases.update(_ + 1))
        )
        resource = wrap(base).run(request)
        _ = assertEquals(
          (metricsFilters, labels, traceFilters, propagationFilters, processed),
          (0, 0, 0, 0, 0),
        )
        _ <- resource.use(_.body.compile.drain)
        _ <- resource.use(_.body.compile.drain)
        seen <- contexts.get
        released <- releases.get
        metrics <- kit.collectMetrics
        spans <- kit.finishedSpans
      } yield {
        assertEquals(
          (metricsFilters, labels, traceFilters, propagationFilters, processed),
          (2, 2, 2, 2, 2),
        )
        assertEquals(released, 2)
        assertEquals(seen.distinct.size, 2)
        MetricExpectations
          .checkAllDistinct(
            metrics,
            MetricExpectation
              .histogram(HttpMetrics.ClientRequestDuration.name)
              .exactlyPoints(PointExpectation.histogram.count(2L)),
            MetricExpectation
              .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
              .exactlyPoints(PointExpectation.numeric(0L)),
          )
          .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
        TraceExpectations
          .check(
            spans,
            TraceForestExpectation.unordered(
              TraceExpectation.leaf(SpanExpectation.client("GET").spanContextExact(seen(0))),
              TraceExpectation.leaf(SpanExpectation.client("GET").spanContextExact(seen(1))),
            ),
          )
          .fold(mismatches => fail(TraceExpectations.format(mismatches)), identity)
      }
    }
  }

  private def filter(enabled: Boolean): PerRequestFilter =
    if (enabled) PerRequestFilter.alwaysEnabled else PerRequestFilter.neverEnabled

}
