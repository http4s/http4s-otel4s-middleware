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
import cats.effect.Ref
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.BuildInfo
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.SpanDataProvider
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.testkit.InstrumentationScopeExpectation
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.ExemplarExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanContextExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.sdk.trace.samplers.Sampler
import org.typelevel.otel4s.semconv.SchemaUrls
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.SpanContext

class ServerInstrumentationPoliciesTest extends CatsEffectSuite {
  private val httpExpectation: SpanExpectation = SpanExpectation
    .server("GET")
    .scope(
      InstrumentationScopeExpectation
        .name("org.http4s.otel4s.middleware.server")
        .version(BuildInfo.version)
        .schemaUrl(SchemaUrls.V1_43_0)
    )

  private val provider = ServerSpanDataProvider.openTelemetry(
    new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {}
  )
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
    test(s"independent server policies: trace=$trace, meter=$meter, reverse=$propagate") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit
          .inMemory[IO](_.addTextMapPropagators(W3CTraceContextPropagator.default))
          .use { kit =>
            var metricChecks = 0
            var traceChecks = 0
            var propagationChecks = 0
            val metricFilter: PerRequestFilter = _ => { metricChecks += 1; enabled(meter) }
            val traceFilter: PerRequestFilter = _ => { traceChecks += 1; enabled(trace) }
            val propagationFilter: PerRequestFilter = _ => {
              propagationChecks += 1; enabled(propagate)
            }
            for {
              tracer <- kit.tracerProvider.get("test")
              http <- Ref.of[IO, Option[SpanContext]](None)
              producer <- Ref.of[IO, Option[SpanContext]](None)
              cleanup <- Ref.of[IO, Option[SpanContext]](None)
              responseHeaders <- Ref.of[IO, Headers](Headers.empty)
              wrap <- createMiddleware(
                kit,
                if (trace) provider else excludedProvider,
                metricsConfig = ServerMetricsConfig.default.optIntoActiveRequests
                  .withRequestFilter(metricFilter)
                  .withRequestAttributes { _ => assert(meter); Attributes.empty },
                tracingFilter = traceFilter,
                reversePropagationFilter = propagationFilter,
              )
              body = (Stream.exec(tracer.currentSpanContext.flatMap(producer.set)) ++
                Stream.emit(1.toByte).covary[IO])
                .onFinalize(tracer.currentSpanContext.flatMap(cleanup.set))
              base = HttpApp[IO](_ =>
                tracer.currentSpanContext
                  .flatMap(http.set)
                  .as(
                    Response[IO](headers =
                      Headers("x-existing" -> "keep", "traceparent" -> "existing")
                    )
                      .withBodyStream(body)
                  )
              )
              contexts <- tracer.span("parent").use { parent =>
                tracer.propagate(Headers.empty).flatMap { incoming =>
                  wrap(base)(
                    Request[IO](uri = uri"https://example.test/items", headers = incoming)
                  ).flatMap { response =>
                    responseHeaders.set(response.headers) >> tracer.span("consumer").use {
                      consumer =>
                        val consume = response.body
                          .evalMap { byte =>
                            tracer.currentSpanContext.map { current =>
                              assertEquals(current, Some(consumer.context)); byte
                            }
                          }
                          .compile
                          .drain
                        (consume >> consume).as((parent.context, consumer.context))
                    }
                  }
                }
              }
              (parent, consumer) = contexts
              context <- http.get.map(_.get)
              source <- producer.get
              finalizer <- cleanup.get
              headers <- responseHeaders.get
              metrics <- kit.collectMetrics
              spans <- kit.finishedSpans
            } yield {
              assertEquals(metricChecks, 1)
              assertEquals(traceChecks, 1)
              assertEquals(propagationChecks, if (trace) 1 else 0)
              if (trace) assertNotEquals(context, parent) else assertEquals(context, parent)
              val expectedSource = if (trace || meter) context else consumer
              assertEquals(source, Some(expectedSource))
              assertEquals(finalizer, Some(expectedSource))
              assertEquals(headers.get(ci"x-existing").map(_.head.value), Some("keep"))
              assertEquals(
                headers.get(ci"traceparent").map(_.head.value),
                Some(
                  if (trace && propagate)
                    s"00-${context.traceIdHex}-${context.spanIdHex}-${context.traceFlags.toHex}"
                  else "existing"
                ),
              )
              if (meter)
                MetricExpectations
                  .checkAllDistinct(
                    metrics,
                    MetricExpectation
                      .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                      .exactlyPoints(PointExpectation.numeric(0L)),
                    MetricExpectation
                      .histogram(HttpMetrics.ServerRequestDuration.name)
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
              else
                List(
                  HttpMetrics.ServerRequestDuration.name,
                  HttpExperimentalMetrics.ServerActiveRequests.name,
                ).foreach(name =>
                  assertEquals(
                    MetricExpectations.find(metrics, MetricExpectation.name(name)),
                    None,
                  )
                )
              val children = List(
                TraceExpectation
                  .leaf(SpanExpectation.name("consumer").spanContextExact(consumer))
              ) ++
                Option.when(trace)(
                  TraceExpectation.leaf(
                    httpExpectation
                      .spanContextExact(context)
                      .parentSpanContext(SpanContextExpectation.exact(parent).remote(true))
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
  test("unsampled HTTP context reaches body and response propagation without exported exemplars") {
    OpenTelemetrySdkTestkit
      .inMemory[IO](
        _.addTextMapPropagators(W3CTraceContextPropagator.default)
          .addTracerProviderCustomizer(_.withSampler(Sampler.alwaysOff))
      )
      .use { kit =>
        for {
          tracer <- kit.tracerProvider.get("test")
          http <- Ref.of[IO, Option[SpanContext]](None)
          producer <- Ref.of[IO, Option[SpanContext]](None)
          wrap <- ServerInstrumentation.middleware[IO](
            kit,
            ServerMetricsConfig.default.optIntoActiveRequests,
            ServerTracingConfig(provider)
              .withReversePropagationFilter(PerRequestFilter.alwaysEnabled),
          )
          body = Stream.exec(tracer.currentSpanContext.flatMap(producer.set))
          response <- wrap(
            HttpApp[IO](_ =>
              tracer.currentSpanContext
                .flatMap(http.set)
                .as(Response[IO](headers = Headers.empty).withBodyStream(body))
            )
          )(Request[IO](uri = uri"https://example.test/items"))
          _ <- response.body.compile.drain
          context <- http.get.map(_.get)
          observed <- producer.get
          metrics <- kit.collectMetrics
          spans <- kit.finishedSpans
        } yield {
          assert(!context.isSampled)
          assertEquals(observed, Some(context))
          assertEquals(
            response.headers.get(ci"traceparent").map(_.head.value),
            Some(s"00-${context.traceIdHex}-${context.spanIdHex}-${context.traceFlags.toHex}"),
          )
          MetricExpectations
            .checkAllDistinct(
              metrics,
              MetricExpectation
                .histogram(HttpMetrics.ServerRequestDuration.name)
                .exactlyPoints(PointExpectation.histogram.count(1L).exemplarCount(0)),
              MetricExpectation
                .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                .exactlyPoints(PointExpectation.numeric(0L)),
            )
            .fold(m => fail(MetricExpectations.format(m)), identity)
          TraceExpectations
            .check(spans, TraceForestExpectation.empty)
            .fold(m => fail(TraceExpectations.format(m)), identity)
        }
      }
  }

  test(
    "a reused effect creates independent exchanges and evaluates policies once per invocation"
  ) {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
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
      val metricsConfig = ServerMetricsConfig.default.optIntoActiveRequests
        .withRequestFilter { _ => metricsFilters += 1; PerRequestConfig.Enabled }
        .withRequestAttributes { _ => labels += 1; Attributes.empty }
      val tracingConfig = ServerTracingConfig(countingProvider)
        .withRequestFilter { _ => traceFilters += 1; PerRequestConfig.Enabled }
        .withReversePropagationFilter { _ => propagationFilters += 1; PerRequestConfig.Enabled }
      for {
        tracer <- kit.tracerProvider.get("test")
        contexts <- Ref.of[IO, Vector[SpanContext]](Vector.empty)
        releases <- Ref.of[IO, Int](0)
        wrap <- ServerInstrumentation.middleware[IO](kit, metricsConfig, tracingConfig)
        base = HttpApp[IO](_ =>
          tracer.currentSpanContext
            .flatMap(ctx => contexts.update(_ :+ ctx.get))
            .as(Response[IO]().withBodyStream(Stream.empty.onFinalize(releases.update(_ + 1))))
        )
        effect = wrap(base)(Request[IO](uri = uri"https://example.test/items"))
        _ = assertEquals(
          (metricsFilters, labels, traceFilters, propagationFilters, processed),
          (0, 0, 0, 0, 0),
        )
        _ <- effect.flatMap(_.body.compile.drain)
        _ <- effect.flatMap(_.body.compile.drain)
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
              .histogram(HttpMetrics.ServerRequestDuration.name)
              .exactlyPoints(PointExpectation.histogram.count(2L)),
            MetricExpectation
              .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
              .exactlyPoints(PointExpectation.numeric(0L)),
          )
          .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
        TraceExpectations
          .check(
            spans,
            TraceForestExpectation.unordered(
              TraceExpectation.leaf(SpanExpectation.server("GET").spanContextExact(seen(0))),
              TraceExpectation.leaf(SpanExpectation.server("GET").spanContextExact(seen(1))),
            ),
          )
          .fold(mismatches => fail(TraceExpectations.format(mismatches)), identity)
      }
    }
  }

  List(
    "metrics initialization",
    "app construction",
    "metrics classification",
    "trace classification",
  ).foreach { failure =>
    test(s"$failure failure still ends the HTTP span") {
      val boom = new IllegalStateException("callback failure")
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        val metricsConfig = ServerMetricsConfig.default.optIntoActiveRequests
          .withRequestAttributes { _ =>
            if (failure == "metrics initialization") throw boom else Attributes.empty
          }
          .withErrorClassifier { (_, _) =>
            if (failure == "metrics classification") throw boom else false
          }
        val tracingConfig = ServerTracingConfig(provider).withErrorClassifier { (_, _) =>
          if (failure == "trace classification") throw boom else false
        }
        for {
          calls <- Ref.of[IO, Int](0)
          cleanups <- Ref.of[IO, Int](0)
          wrap <- ServerInstrumentation.middleware[IO](kit, metricsConfig, tracingConfig)
          base = HttpApp[IO](_ =>
            if (failure == "app construction") throw boom
            else
              calls
                .update(_ + 1)
                .as(
                  Response[IO]().withBodyStream(Stream.empty.onFinalize(cleanups.update(_ + 1)))
                )
          )
          result <- wrap(base)(Request[IO](uri = uri"https://example.test/items"))
            .flatMap(_.body.compile.drain)
            .attempt
          invoked <- calls.get
          cleaned <- cleanups.get
          metrics <- kit.collectMetrics
          spans <- kit.finishedSpans
        } yield {
          assertEquals(result, Left(boom))
          assertEquals(
            invoked,
            if (Set("metrics initialization", "app construction")(failure)) 0 else 1,
          )
          assertEquals(cleaned, invoked)
          TraceExpectations
            .check(
              spans,
              TraceForestExpectation.unordered(
                TraceExpectation.leaf(SpanExpectation.server("GET"))
              ),
            )
            .fold(m => fail(TraceExpectations.format(m)), identity)
          if (failure == "metrics initialization") {
            List(
              HttpMetrics.ServerRequestDuration.name,
              HttpExperimentalMetrics.ServerActiveRequests.name,
            )
              .foreach(name =>
                assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(name)), None)
              )
          } else {
            MetricExpectations
              .checkAllDistinct(
                metrics,
                MetricExpectation
                  .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
                  .exactlyPoints(PointExpectation.numeric(0L)),
              )
              .fold(m => fail(MetricExpectations.format(m)), identity)
            if (failure == "trace classification")
              MetricExpectations
                .checkAllDistinct(
                  metrics,
                  MetricExpectation
                    .histogram(HttpMetrics.ServerRequestDuration.name)
                    .exactlyPoints(PointExpectation.histogram.count(1L).exemplarCount(1)),
                )
                .fold(m => fail(MetricExpectations.format(m)), identity)
          }
        }
      }
    }
  }

  private def createMiddleware(
      kit: OpenTelemetrySdkTestkit[IO],
      provider: SpanDataProvider,
      metricsConfig: ServerMetricsConfig,
      tracingFilter: PerRequestFilter,
      reversePropagationFilter: PerRequestFilter,
  ): IO[HttpApp[IO] => HttpApp[IO]] = ServerInstrumentation.middleware[IO](
    kit,
    metricsConfig,
    ServerTracingConfig(provider)
      .withRequestFilter(tracingFilter)
      .withReversePropagationFilter(reversePropagationFilter),
  )

  private def enabled(value: Boolean): PerRequestConfig =
    if (value) PerRequestConfig.Enabled else PerRequestConfig.Disabled

}
