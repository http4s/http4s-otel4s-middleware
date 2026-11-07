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
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.syntax.literals._
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.trace.TracerProvider

class SensitiveQueryRedactionTest extends CatsEffectSuite {
  private val uri =
    uri"https://user:password@example.test/items?color=blue&sig=first&sig=second&X-Amz-Signature=aws&X-Amz-Credential=credential&X-Amz-Security-Token=token&X-Goog-Signature=google&empty=&flag#fragment"
  private val redacted =
    uri"https://REDACTED:REDACTED@example.test/items?color=blue&sig=REDACTED&sig=REDACTED&X-Amz-Signature=REDACTED&X-Amz-Credential=REDACTED&X-Amz-Security-Token=REDACTED&X-Goog-Signature=REDACTED&empty=&flag#fragment"

  test("standalone client records a redacted URL without rewriting the request") {
    checkClient(combined = false)
  }

  test("combined client records a redacted URL without rewriting the request") {
    checkClient(combined = true)
  }

  test("standalone server records a redacted query without rewriting the request") {
    checkServer(combined = false)
  }

  test("combined server records a redacted query without rewriting the request") {
    checkServer(combined = true)
  }

  private def checkClient(combined: Boolean): IO[Unit] =
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
      implicit val contextManager: ContextManager[IO] = kit.contextManager
      implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
      val provider = ClientSpanDataProvider.openTelemetry(
        new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive {}
      )
      val base = Client[IO](request =>
        Resource.eval(IO {
          assertEquals(request.uri, uri)
          Response[IO]()
        })
      )
      val wrap =
        if (combined)
          ClientInstrumentation
            .middleware[IO](ClientMetricsConfig.default, ClientTracingConfig(provider))
        else ClientMiddleware.builder[IO](provider).build.map(middleware => middleware.wrapClient _)
      for {
        middleware <- wrap
        _ <- middleware(base).run(Request[IO](uri = uri)).use(_.body.compile.drain)
        spans <- kit.finishedSpans
      } yield {
        SpanExpectations
          .checkAllDistinct(
            spans,
            SpanExpectation
              .client("GET")
              .attributesSubset(UrlAttributes.UrlFull(redacted.renderString)),
          )
          .fold(m => fail(SpanExpectations.format(m)), identity)
        assertEquals(spans.size, 1)
      }
    }

  private def checkServer(combined: Boolean): IO[Unit] =
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
      implicit val contextManager: ContextManager[IO] = kit.contextManager
      implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
      val provider = ServerSpanDataProvider.openTelemetry(
        new PathRedactor.NeverRedact with QueryRedactor.RedactSensitive {}
      )
      val base = HttpApp[IO](request =>
        IO {
          assertEquals(request.uri, uri)
          Response[IO]()
        }
      )
      val wrap =
        if (combined)
          ServerInstrumentation
            .middleware[IO](ServerMetricsConfig.default, ServerTracingConfig(provider))
        else
          ServerMiddleware.builder[IO](provider).build.map(middleware => middleware.wrapHttpApp _)
      for {
        middleware <- wrap
        response <- middleware(base)(Request[IO](uri = uri))
        _ <- response.body.compile.drain
        spans <- kit.finishedSpans
      } yield {
        SpanExpectations
          .checkAllDistinct(
            spans,
            SpanExpectation
              .server("GET")
              .attributesSubset(
                UrlAttributes.UrlPath("/items"),
                UrlAttributes.UrlQuery(redacted.query.renderString),
              ),
          )
          .fold(m => fail(SpanExpectations.format(m)), identity)
        assertEquals(spans.size, 1)
      }
    }
}
