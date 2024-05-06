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
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.client._
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.sdk.testkit.trace.TracesTestkit
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer

class ClientMiddlewareTests extends CatsEffectSuite {
  test("ClientMiddleware") {
    TracesTestkit
      .inMemory[IO](
        _.addTextMapPropagators(W3CTraceContextPropagator.default)
      )
      .use { testkit =>
        for {
          tracerIO <- testkit.tracerProvider.get("tracer")
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
          spans <- testkit.finishedSpans
        } yield {
          assertEquals(spans.length, 1)
          val span = spans.head
          assertEquals(span.name, "Http Client - GET")
          assertEquals(span.kind, SpanKind.Client)

          val attributes = span.attributes
          def getAttr[A: AttributeKey.KeySelect](name: String): Option[A] =
            attributes.get[A](name).map(_.value)

          assertEquals(getAttr[String]("http.request.method"), Some("GET"))
          assertEquals(getAttr[String]("url.full"), Some("/"))
          assertEquals(getAttr[String]("url.path"), Some("/"))
          assertEquals(getAttr[String]("url.query"), Some(""))
          assertEquals(getAttr[String]("server.address"), Some("localhost"))
          assertEquals(getAttr[Long]("http.response.status_code"), Some(200L))
          assertEquals(getAttr[String]("exit.case"), Some("succeeded"))
        }
      }
  }
}
