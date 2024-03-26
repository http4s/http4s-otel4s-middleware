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

package org.http4s.otel4s.middleware

import cats.effect.IO
import io.opentelemetry.api.common.{AttributeKey => JAttributeKey}
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator
import io.opentelemetry.context.propagation.{ContextPropagators => JContextPropagators}
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.data.SpanData
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import io.opentelemetry.sdk.{OpenTelemetrySdk => JOpenTelemetrySdk}
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.client._
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.jdk.CollectionConverters._

class ClientMiddlewareTests extends CatsEffectSuite {
  import ClientMiddlewareTests._

  test("ClientMiddleware") {
    for {
      sdk <- makeSdk
      tracerIO <- sdk.provider.get("tracer")
      _ <- {
        implicit val tracer: Tracer[IO] = tracerIO
        val fakeClient =
          Client.fromHttpApp[IO] {
            HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
          }
        val tracedClient = ClientMiddleware.default[IO].build(fakeClient)

        tracedClient
          .run(Request[IO](Method.GET))
          .use(_.body.compile.drain)
      }
      spans <- sdk.finishedSpans
    } yield {
      assertEquals(spans.length, 1)
      val span = spans.head
      assertEquals(span.getName, "Http Client - GET")
      val attributes = span.getAttributes

      def getStringAttribute(name: String): Option[String] =
        Option(attributes.get(JAttributeKey.stringKey(name)))
      def getLongAttribute(name: String): Option[Long] =
        Option(attributes.get(JAttributeKey.longKey(name))).map(_.longValue())

      assertEquals(getStringAttribute("span.kind"), Some("client"))
      assertEquals(getStringAttribute("http.method"), Some("GET"))
      assertEquals(getStringAttribute("http.url"), Some("/"))
      assertEquals(getStringAttribute("http.target"), Some("/"))
      assertEquals(getStringAttribute("http.host"), Some("localhost"))
      assertEquals(getStringAttribute("exit.case"), Some("succeeded"))
      assertEquals(getLongAttribute("http.status_code"), Some(200L))
      assertEquals(getStringAttribute("http.flavor"), Some("1.1"))
    }
  }
}

object ClientMiddlewareTests {
  private def makeSdk: IO[Sdk] = {
    val exporter = InMemorySpanExporter.create()

    val tracerProvider = SdkTracerProvider
      .builder()
      .addSpanProcessor(SimpleSpanProcessor.create(exporter))
      .build()

    val jOtel = JOpenTelemetrySdk
      .builder()
      .setTracerProvider(tracerProvider)
      .setPropagators(
        JContextPropagators.create(W3CTraceContextPropagator.getInstance())
      )
      .build()

    OtelJava.forAsync[IO](jOtel).map { otel4s =>
      new Sdk(otel4s.tracerProvider, exporter)
    }
  }

  final class Sdk(val provider: TracerProvider[IO], exporter: InMemorySpanExporter) {
    def finishedSpans: IO[List[SpanData]] =
      IO.delay(exporter.getFinishedSpanItems.asScala.toList)
  }
}
