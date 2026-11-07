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

package org.http4s.otel4s.middleware.tests

import cats.effect.IO
import cats.effect.Resource
import munit.CatsEffectSuite
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.TypedAttributes
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.trace.TracerProvider

class KnownMethodsTest extends CatsEffectSuite {
  private val lowerGet = Method.fromString("get").toOption.get
  private val cases = List(
    (None, Method.GET, "GET"),
    (None, Method.PROPFIND, "_OTHER"),
    (Some(Set(Method.PROPFIND)), Method.PROPFIND, "PROPFIND"),
    (Some(Set(Method.PROPFIND)), Method.GET, "_OTHER"),
    (Some(Set(lowerGet)), lowerGet, "get"),
    (Some(Set(lowerGet)), Method.GET, "_OTHER"),
    (Some(Set.empty[Method]), Method.GET, "_OTHER"),
    (Some(TypedAttributes.DefaultKnownMethods + Method.PROPFIND), Method.GET, "GET"),
    (Some(TypedAttributes.DefaultKnownMethods + Method.PROPFIND), Method.PROPFIND, "PROPFIND"),
  )

  List("client", "server").foreach { side =>
    List(false, true).foreach { combined =>
      cases.foreach { case (methods, method, expected) =>
        test(s"$side known methods: combined=$combined, override=$methods, method=$method") {
          OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
            implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
            implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
            implicit val contextManager: ContextManager[IO] = kit.contextManager
            val clientDefaults =
              ClientMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize
            val serverDefaults =
              ServerMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize
            val clientConfig = methods.fold(clientDefaults)(clientDefaults.withKnownMethods)
            val serverConfig = methods.fold(serverDefaults)(serverDefaults.withKnownMethods)
            val clientProviderDefaults = ClientSpanDataProvider
              .openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
            val clientProvider = methods
              .fold(clientProviderDefaults)(clientProviderDefaults.withKnownMethods)
              .withUrlTemplateClassifier(_ => Some("/items/{id}"))
            val serverProviderDefaults = ServerSpanDataProvider
              .openTelemetry(new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {})
            val serverProvider = methods
              .fold(serverProviderDefaults)(serverProviderDefaults.withKnownMethods)
              .withRouteClassifier(_ => Some("/items/{id}"))
            val request = Request[IO](method, uri"https://example.test/items/42")
              .putHeaders("Content-Length" -> "0")
            val response = Response[IO]().putHeaders("Content-Length" -> "0")
            val run = if (side == "client") {
              val wrap =
                if (combined)
                  ClientInstrumentation.middleware[IO](
                    clientConfig,
                    ClientTracingConfig(clientProvider),
                  )
                else
                  ClientMetrics.middleware[IO](clientConfig)
              wrap.flatMap(
                _(Client[IO](_ => Resource.pure(response))).run(request).use(_.body.compile.drain)
              )
            } else {
              val wrap =
                if (combined)
                  ServerInstrumentation.middleware[IO](
                    serverConfig,
                    ServerTracingConfig(serverProvider),
                  )
                else
                  ServerMetrics.middleware[IO](serverConfig)
              wrap.flatMap(_(HttpApp.pure[IO](response))(request)).flatMap(_.body.compile.drain)
            }
            for {
              _ <- run
              metrics <- kit.collectMetrics
              spans <- kit.finishedSpans
            } yield {
              val label = HttpAttributes.HttpRequestMethod(expected)
              val histograms = List("request.duration", "request.body.size", "response.body.size")
                .map(suffix =>
                  MetricExpectation
                    .histogram(s"http.$side.$suffix")
                    .exactlyPoints(
                      PointExpectation.histogram.count(1L).attributesSubset(label)
                    )
                )
              val active = MetricExpectation
                .sum[Long](s"http.$side.active_requests")
                .exactlyPoints(PointExpectation.numeric(0L).attributesSubset(label))
              MetricExpectations
                .checkAllDistinct(metrics, (active :: histograms): _*)
                .fold(m => fail(MetricExpectations.format(m)), identity)
              if (combined) {
                val name = s"${if (expected == "_OTHER") "HTTP" else expected} /items/{id}"
                val span = (if (side == "client") SpanExpectation.client(name)
                            else SpanExpectation.server(name))
                  .attributesSubset(label)
                  .where("original method is present if and only if normalization changes it") {
                    data =>
                      data.attributes.elements
                        .get(HttpAttributes.HttpRequestMethodOriginal)
                        .map(_.value) ==
                        Option.when(expected != method.name)(method.name)
                  }
                SpanExpectations
                  .checkAllDistinct(spans, span)
                  .fold(m => fail(SpanExpectations.format(m)), identity)
                assertEquals(spans.size, 1)
              } else assertEquals(spans.size, 0)
              assertEquals(clientDefaults.knownMethods, TypedAttributes.DefaultKnownMethods)
              assertEquals(serverDefaults.knownMethods, TypedAttributes.DefaultKnownMethods)
            }
          }
        }
      }
    }
  }
}
