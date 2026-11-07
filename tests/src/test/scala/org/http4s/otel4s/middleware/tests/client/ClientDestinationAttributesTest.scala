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
import cats.effect.Resource
import munit.CatsEffectSuite
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.headers.Host
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.syntax.literals._
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectations
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.trace.TracerProvider

class ClientDestinationAttributesTest extends CatsEffectSuite {
  private val cases = List(
    (uri"/items", Some("example.test:8443"), Some("example.test"), Some(8443L)),
    (uri"/items", Some("example.test"), Some("example.test"), None),
    (uri"/items", Some("[2001:db8::1]:8443"), Some("2001:db8::1"), Some(8443L)),
    (uri"https:/items", Some("example.test:8443"), Some("example.test"), Some(8443L)),
    (uri"https:/items", Some("example.test"), Some("example.test"), Some(443L)),
    (uri"https://origin.test/items", Some("other.test:9000"), Some("origin.test"), Some(443L)),
    (uri"//origin.test/items", Some("other.test:9000"), Some("origin.test"), None),
    (uri"/items", Some("bad host:invalid"), None, None),
    (uri"/items", None, None, None),
  )

  List(false, true).foreach { combined =>
    cases.foreach { case (uri, host, address, port) =>
      test(s"destination attributes: combined=$combined, uri=$uri, Host=$host") {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          implicit val meterProvider: MeterProvider[IO] = kit.meterProvider
          implicit val contextManager: ContextManager[IO] = kit.contextManager
          implicit val tracerProvider: TracerProvider[IO] = kit.tracerProvider
          val provider = ClientSpanDataProvider.openTelemetry(new UriRedactor.OnlyRedactUserInfo {})
          val config =
            ClientMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize
          val initial = Request[IO](uri = uri).putHeaders("Content-Length" -> "0")
          val request = host.fold(initial)(value => initial.putHeaders("Host" -> value))
          val base = Client[IO](received =>
            Resource.eval(IO {
              assertEquals(received.uri, request.uri)
              assertEquals(received.headers.get[Host], request.headers.get[Host])
              Response[IO]().putHeaders("Content-Length" -> "0")
            })
          )
          val wrap =
            if (combined)
              ClientInstrumentation.middleware[IO](config, ClientTracingConfig(provider))
            else
              for {
                metrics <- ClientMetrics
                  .middleware[IO](config)
                tracing <- ClientMiddleware.builder[IO](provider).build
              } yield (client: Client[IO]) => tracing.wrapClient(metrics(client))
          for {
            middleware <- wrap
            _ <- middleware(base).run(request).use(_.body.compile.drain)
            metrics <- kit.collectMetrics
            spans <- kit.finishedSpans
          } yield {
            val labels = Attributes(HttpAttributes.HttpRequestMethod("GET")) ++
              address.map(ServerAttributes.ServerAddress(_)) ++ port.map(
                ServerAttributes.ServerPort(_)
              )
            val terminal = labels ++ Attributes(
              HttpAttributes.HttpResponseStatusCode(200L),
              NetworkAttributes.NetworkProtocolVersion("1.1"),
            )
            val active = MetricExpectation
              .sum[Long]("http.client.active_requests")
              .exactlyPoints(PointExpectation.numeric(0L).attributesExact(labels))
            val histograms = List("request.duration", "request.body.size", "response.body.size")
              .map(suffix =>
                MetricExpectation
                  .histogram(s"http.client.$suffix")
                  .exactlyPoints(PointExpectation.histogram.count(1L).attributesExact(terminal))
              )
            MetricExpectations
              .checkAllDistinct(metrics, (active :: histograms): _*)
              .fold(m => fail(MetricExpectations.format(m)), identity)
            SpanExpectations
              .checkAllDistinct(
                spans,
                SpanExpectation
                  .client("GET")
                  .where("destination attributes agree with the metrics") { span =>
                    span.attributes.elements
                      .get(ServerAttributes.ServerAddress)
                      .map(_.value) == address &&
                    span.attributes.elements.get(ServerAttributes.ServerPort).map(_.value) == port
                  },
              )
              .fold(m => fail(SpanExpectations.format(m)), identity)
            assertEquals(spans.size, 1)
          }
        }
      }
    }
  }
}
