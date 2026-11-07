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

package org.http4s.otel4s.middleware.server

import cats.effect.IO
import cats.effect.Ref
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.Response
import org.http4s.otel4s.middleware.PerRequestConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.trace.SpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics
import org.typelevel.otel4s.trace.Tracer

/** Checks that disabled tracing skips its filters and callbacks. */
class ServerInstrumentationAvailabilityTest extends CatsEffectSuite {
  private val provider = new SpanDataProvider {
    type Shared = Unit
    def processSharedData[F[_]](request: Request[F]): Unit = fail("disabled span data")
    def spanName[F[_]](request: Request[F], shared: Unit): String = fail("disabled span name")
    def requestAttributes[F[_]](request: Request[F], shared: Unit): Attributes = fail(
      "disabled span attributes"
    )
    def responseAttributes[F[_]](response: Response[F]): Attributes = fail("disabled span response")
    def exceptionAttributes(cause: Throwable): Attributes = fail("disabled span error")
  }

  List(false, true).foreach { metered =>
    test(
      s"disabled tracing skips all tracing policies and retains HTTP behavior: metered=$metered"
    ) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
        implicit val contextManager: ContextManager[IO] = kit.contextManager
        implicit val tracer: Tracer[IO] = Tracer.noop[IO]
        var filters = 0
        val metricsConfig = ServerMetricsConfig.default.optIntoActiveRequests
          .withRequestFilter { _ =>
            filters += 1; if (metered) PerRequestConfig.Enabled else PerRequestConfig.Disabled
          }
          .withRequestAttributes { _ =>
            if (!metered) fail("excluded metrics labels"); Attributes.empty
          }
        val tracingConfig = ServerTracingConfig(provider)
          .withRequestFilter(_ => fail("disabled tracing filter"))
          .withReversePropagationFilter(_ => fail("disabled propagation filter"))
          .withErrorClassifier((_, _) => fail("disabled trace classification"))
        for {
          effects <- Ref.of[IO, Int](0)
          request = Request[IO](uri = uri"https://example.test/items")
            .withBodyStream(Stream.exec(effects.update(_ + 1)))
          original = Response[IO]().withBodyStream(Stream.exec(effects.update(_ + 1)))
          wrap <- ServerInstrumentationMiddleware.create[IO](
            metricsConfig,
            tracingConfig,
          )
          base = HttpApp[IO] { req =>
            assert(req eq request)
            req.body.compile.drain.as(original)
          }
          response <- wrap(base)(request)
          _ <- {
            if (!metered) assert(response eq original)
            response.body.compile.drain
          }
          count <- effects.get
          metrics <- kit.collectMetrics
          spans <- kit.finishedSpans
        } yield {
          assertEquals(filters, 1)
          assertEquals(count, 2)
          if (metered)
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
              .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
          else
            List(
              HttpMetrics.ServerRequestDuration.name,
              HttpExperimentalMetrics.ServerActiveRequests.name,
            )
              .foreach(name =>
                assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(name)), None)
              )
          TraceExpectations
            .check(spans, TraceForestExpectation.empty)
            .fold(mismatches => fail(TraceExpectations.format(mismatches)), identity)
        }
      }
    }
  }
}
