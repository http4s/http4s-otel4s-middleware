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
import fs2.Stream
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
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
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.TracerProvider

class ClientBodySizeExemplarTest extends CatsEffectSuite {
  private val requestSizeName = HttpExperimentalMetrics.ClientRequestBodySize.name
  private val responseSizeName = HttpExperimentalMetrics.ClientResponseBodySize.name
  private val buckets = BucketBoundaries(0d, 3d, 7d, 15d)
  private val request = Request[IO](Method.POST, uri"https://example.test/items")
    .withBodyStream(Stream.emits(Vector[Byte](1, 2, 3)).covary[IO])
    .putHeaders(Header.Raw(ci"Content-Length", "9"))
  private val terminal = Attributes(
    HttpAttributes.HttpRequestMethod("POST"),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
    HttpAttributes.HttpResponseStatusCode(200L),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

  List(false, true).foreach { observed =>
    List(false, true).foreach { unread =>
      test(
        s"body sizes have HTTP-span exemplars before span completion: observe=$observed unread=$unread"
      ) {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          implicit val provider: TracerProvider[IO] = kit.tracerProvider
          val sizeConfig =
            if (observed) BodySizeConfig.observe(buckets)
            else BodySizeConfig.contentLength(buckets)
          val config = ClientMetricsConfig.default
            .optIntoRequestBodySize(sizeConfig)
            .optIntoResponseBodySize(sizeConfig)
          for {
            middleware <- ClientMetrics.middleware[IO](kit, config)
            tracing <- ClientMiddleware
              .builder[IO](
                ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
              )
              .build
            tracer <- kit.tracerProvider.get("test")
            context <- Ref.of[IO, Option[SpanContext]](None)
            beforeResponse <- Ref.of[IO, List[MetricData]](Nil)
            beforeRelease <- Ref.of[IO, List[MetricData]](Nil)
            spansAtRelease <- Ref.of[IO, List[SpanData]](Nil)
            base = Client[IO] { req =>
              Resource.make(
                req.body.compile.drain >> tracer.currentSpanContext.flatMap(context.set) >>
                  kit.collectMetrics
                    .flatMap(beforeResponse.set)
                    .as(
                      Response[IO]()
                        .withBodyStream(Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO])
                        .putHeaders(Header.Raw(ci"Content-Length", "11"))
                    )
              )(_ =>
                kit.collectMetrics.flatMap(beforeRelease.set) >>
                  kit.finishedSpans.flatMap(spansAtRelease.set)
              )
            }
            _ <- tracing.wrapClient(middleware(base)).run(request).use { response =>
              if (unread) IO.unit else response.body.compile.drain
            }
            http <- context.get.map(_.get)
            during <- beforeResponse.get
            released <- beforeRelease.get
            open <- spansAtRelease.get
            finalMetrics <- kit.collectMetrics
          } yield {
            List(requestSizeName, responseSizeName).foreach { name =>
              assertEquals(MetricExpectations.find(during, MetricExpectation.name(name)), None)
            }
            val sizes =
              List(requestSizeName -> (if (observed) 3L else 9L)) ++
                Option.unless(observed && unread)(
                  responseSizeName -> (if (observed) 5L else 11L)
                )
            assertMetrics(
              released,
              sizes.map { case (name, size) =>
                sizeExpectation(name, size, Some(http))
              }: _*
            )
            // Collection drains the exemplar reservoir; cumulative counts must still remain one.
            assertMetrics(
              finalMetrics,
              sizes.map { case (name, size) =>
                sizeExpectation(name, size, None)
              }: _*
            )
            if (observed && unread)
              assertEquals(
                MetricExpectations.find(finalMetrics, MetricExpectation.name(responseSizeName)),
                None,
              )
            TraceExpectations
              .check(open, TraceForestExpectation.empty)
              .fold(mismatches => fail(TraceExpectations.format(mismatches)), identity)
          }
        }
      }
    }
  }

  private def sizeExpectation(
      name: String,
      value: Long,
      exemplar: Option[SpanContext],
  ): MetricExpectation = {
    val point = PointExpectation.histogram
      .count(1L)
      .sum(value.toDouble)
      .boundaries(buckets)
      .attributesExact(terminal)
      .exemplarCount(exemplar.size)
    MetricExpectation
      .histogram(name)
      .unit("By")
      .exactlyPoints(
        exemplar.fold(point) { http =>
          point.containsExemplars(
            ExemplarExpectation
              .numeric(value.toDouble)
              .traceContext(TraceContext(http.traceId, http.spanId, http.isSampled))
          )
        }
      )
  }

  private def assertMetrics(metrics: List[MetricData], expectations: MetricExpectation*)(implicit
      loc: Location
  ): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
}
