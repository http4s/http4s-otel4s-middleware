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
import org.http4s.Header
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.sdk.testkit.trace.TracesTestkit
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer

class ClientMiddlewareTests extends CatsEffectSuite {
  test("ClientMiddleware") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          tracerIO <- testkit.tracerProvider.get("tracer")
          _ <- {
            implicit val tracer: Tracer[IO] = tracerIO
            val headers =
              Headers(Header.Raw(ci"foo", "bar"), Header.Raw(ci"baz", "qux"))
            val response = Response[IO](Status.Ok).withHeaders(headers)
            val fakeClient =
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            val tracedClient =
              ClientMiddleware
                .default[IO]
                .withAllowedRequestHeaders(Set(ci"foo"))
                .withAllowedResponseHeaders(Set(ci"baz"))
                .build(fakeClient)

            val request =
              Request[IO](Method.GET, uri"http://localhost/?#")
                .withHeaders(headers)
            tracedClient.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield {
          assertEquals(spans.length, 1)
          val span = spans.head
          assertEquals(span.name, "Http Client - GET")
          assertEquals(span.kind, SpanKind.Client)

          val attributes = span.attributes
          assertEquals(attributes.size, 11)
          def getAttr[A: AttributeKey.KeySelect](name: String): Option[A] =
            attributes.get[A](name).map(_.value)

          assertEquals(getAttr[String]("http.request.method"), Some("GET"))
          assertEquals(getAttr[Seq[String]]("http.request.header.foo"), Some(Seq("bar")))
          assertEquals(getAttr[Seq[String]]("http.request.header.baz"), None)
          assertEquals(getAttr[String]("url.full"), Some("http://localhost/?#"))
          assertEquals(getAttr[String]("url.scheme"), Some("http"))
          assertEquals(getAttr[String]("url.path"), Some("/"))
          assertEquals(getAttr[String]("url.query"), Some(""))
          assertEquals(getAttr[String]("url.fragment"), Some(""))
          assertEquals(getAttr[String]("server.address"), Some("localhost"))
          assertEquals(getAttr[Long]("http.response.status_code"), Some(200L))
          assertEquals(getAttr[Seq[String]]("http.response.header.foo"), None)
          assertEquals(getAttr[Seq[String]]("http.response.header.baz"), Some(Seq("qux")))
          assertEquals(getAttr[String]("exit.case"), Some("succeeded"))
        }
      }
  }

  test("ClientMiddleware allows manipulating spans from inner clients") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          tracerIO <- testkit.tracerProvider.get("tracer")
          _ <- {
            implicit val tracer: Tracer[IO] = tracerIO
            val response = Response[IO](Status.Ok)
            val fakeClient =
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            val traceManipulatingClient = Client[IO] { req =>
              fakeClient
                .run(req)
                .evalTap(_ => Tracer[IO].currentSpanOrThrow.flatMap(_.updateName("NEW SPAN NAME")))
            }
            val tracedClient = ClientMiddleware.default[IO].build(traceManipulatingClient)

            val request = Request[IO](Method.GET, uri"http://localhost/?#")
            tracedClient.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield {
          assertEquals(spans.length, 1)
          val span = spans.head
          assertEquals(span.name, "NEW SPAN NAME")
        }
      }
  }

  test("ClientMiddleware allows overriding span name") {
    val spanName = "Overridden span name"
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          tracerIO <- testkit.tracerProvider.get("tracer")
          _ <- {
            implicit val tracer: Tracer[IO] = tracerIO
            val response = Response[IO](Status.Ok)
            val fakeClient =
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            val tracedClient =
              ClientMiddleware
                .default[IO]
                .build(fakeClient)

            val request = Request[IO](Method.GET, uri"http://localhost/?#")
              .withAttribute(ClientMiddleware.OverrideSpanNameKey, spanName)
            tracedClient.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield {
          assertEquals(spans.length, 1)
          val span = spans.head
          assertEquals(span.name, spanName)
        }
      }
  }
}
