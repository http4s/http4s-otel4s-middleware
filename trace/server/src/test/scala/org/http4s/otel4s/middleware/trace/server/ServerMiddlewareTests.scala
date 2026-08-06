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

package org.http4s
package otel4s.middleware.trace.server

import cats.effect.IO
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.testkit.trace.TracesTestkit
import org.typelevel.otel4s.sdk.trace.SpanLimits
import org.typelevel.otel4s.sdk.trace.data.EventData
import org.typelevel.otel4s.sdk.trace.data.LimitedData
import org.typelevel.otel4s.sdk.trace.data.StatusData
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration._
import scala.util.control.NoStackTrace

class ServerMiddlewareTests extends CatsEffectSuite {

  private val spanLimits = SpanLimits.default

  test("ServerMiddleware") {
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
            val tracedServer =
              ServerMiddleware
                .default[IO]
                .withAllowedRequestHeaders(Set(ci"foo"))
                .withAllowedResponseHeaders(Set(ci"baz"))
                .buildHttpApp(HttpApp[IO](_.body.compile.drain.as(response)))

            val request =
              Request[IO](Method.GET, uri"http://localhost/?#")
                .withHeaders(headers)
            tracedServer.run(request).flatMap(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield {
          assertEquals(spans.length, 1)
          val span = spans.head
          assertEquals(span.name, "Http Server - GET")
          assertEquals(span.kind, SpanKind.Server)
          assertEquals(span.status, StatusData.Unset)

          val attributes = span.attributes.elements
          assertEquals(attributes.size, 10)
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
        }
      }
  }

  test("record an exception thrown by the server") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          testkit.tracerProvider.get("tracer").flatMap { implicit tracer =>
            val error = new RuntimeException("oops") with NoStackTrace {}

            val tracedServer = ServerMiddleware
              .default[IO]
              .buildHttpApp(HttpApp[IO](_ => IO.raiseError(error)))

            val request = Request[IO](Method.GET, uri"http://localhost/")

            val events = Vector(
              EventData.fromException(
                Duration.Zero,
                error,
                LimitedData
                  .attributes(spanLimits.maxNumberOfAttributes, spanLimits.maxAttributeValueLength),
                escaped = false,
              )
            )

            val status = StatusData(StatusCode.Error)

            val attributes = Attributes(
              Attribute("http.request.method", "GET"),
              Attribute("url.path", "/"),
              Attribute("url.full", "http://localhost/"),
              Attribute("url.scheme", "http"),
              Attribute("server.address", "localhost"),
              Attribute("error.type", error.getClass.getName),
            )

            for {
              _ <- tracedServer.run(request).flatMap(_.body.compile.drain).attempt
              spans <- testkit.finishedSpans
            } yield {
              assertEquals(spans.map(_.attributes.elements), List(attributes))
              assertEquals(spans.map(_.events.elements), List(events))
              assertEquals(spans.map(_.status), List(status))
            }
          }
        }
    }
  }

  test("record error.type on error response 5xx") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          testkit.tracerProvider.get("tracer").flatMap { implicit tracer =>
            val tracedServer = ServerMiddleware
              .default[IO]
              .buildHttpApp(HttpApp[IO](_ => IO.pure(Response[IO](Status.InternalServerError))))

            val request = Request[IO](Method.GET, uri"http://localhost/")
            val status = StatusData(StatusCode.Error)

            val attributes = Attributes(
              Attribute("http.request.method", "GET"),
              Attribute("http.response.status_code", 500L),
              Attribute("url.path", "/"),
              Attribute("url.full", "http://localhost/"),
              Attribute("url.scheme", "http"),
              Attribute("server.address", "localhost"),
              Attribute("error.type", "500"),
            )

            for {
              _ <- tracedServer.run(request).flatMap(_.body.compile.drain).attempt
              spans <- testkit.finishedSpans
            } yield {
              assertEquals(spans.map(_.attributes.elements), List(attributes))
              assertEquals(spans.map(_.status), List(status))
            }
          }
        }
    }
  }

  test("record cancelation caused by the server") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          testkit.tracerProvider.get("tracer").flatMap { implicit tracer =>
            val tracedServer = ServerMiddleware
              .default[IO]
              .buildHttpApp(HttpApp[IO](_ => IO.canceled.as(Response[IO](Status.Ok))))

            val request = Request[IO](Method.GET, uri"http://localhost/")

            val status = StatusData(StatusCode.Error, "canceled")

            val attributes = Attributes(
              Attribute("http.request.method", "GET"),
              Attribute("url.path", "/"),
              Attribute("url.full", "http://localhost/"),
              Attribute("url.scheme", "http"),
              Attribute("server.address", "localhost"),
            )

            for {
              f <- tracedServer.run(request).flatMap(_.body.compile.drain).start
              _ <- f.joinWithUnit
              spans <- testkit.finishedSpans
            } yield {
              assertEquals(spans.map(_.attributes.elements), List(attributes))
              assertEquals(spans.flatMap(_.events.elements), Nil)
              assertEquals(spans.map(_.status), List(status))
            }
          }
        }
    }
  }

  test("propagate span context to response body streaming") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        testkit.tracerProvider.get("tracer").flatMap { implicit tracer =>
          val body = fs2.Stream.eval(
            Tracer[IO].span("body-span").use(_ => IO.pure('.'.toByte))
          )

          val tracedServer = ServerMiddleware
            .default[IO]
            .buildHttpApp(HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok).withBodyStream(body))))

          val request = Request[IO](Method.GET, uri"http://localhost/")

          for {
            res <- tracedServer.run(request)
            _ <- res.body.compile.drain
            spans <- testkit.finishedSpans
          } yield {
            assertEquals(spans.length, 2)
            val serverSpan = spans.find(_.name == "Http Server - GET")
            val bodySpan = spans.find(_.name == "body-span")

            assert(serverSpan.isDefined, "server span not found")
            assert(bodySpan.isDefined, "body span not found")

            assertEquals(
              bodySpan.get.parentSpanContext.map(_.spanId),
              Some(serverSpan.get.spanContext.spanId),
            )
          }
        }
      }
  }

  test("record an exception thrown during response body streaming") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          testkit.tracerProvider.get("tracer").flatMap { implicit tracer =>
            val error = new RuntimeException("stream crash") with NoStackTrace {}
            val body = fs2.Stream.raiseError[IO](error)

            val tracedServer = ServerMiddleware
              .default[IO]
              .buildHttpApp(HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok).withBodyStream(body))))

            val request = Request[IO](Method.GET, uri"http://localhost/")

            val events = Vector(
              EventData.fromException(
                Duration.Zero,
                error,
                LimitedData
                  .attributes(spanLimits.maxNumberOfAttributes, spanLimits.maxAttributeValueLength),
                escaped = false,
              )
            )

            val status = StatusData(StatusCode.Error)

            val attributes = Attributes(
              Attribute("http.request.method", "GET"),
              Attribute("url.path", "/"),
              Attribute("url.full", "http://localhost/"),
              Attribute("url.scheme", "http"),
              Attribute("server.address", "localhost"),
              Attribute("http.response.status_code", 200L),
              Attribute("error.type", error.getClass.getName),
            )

            for {
              res <- tracedServer.run(request)
              _ <- res.body.compile.drain.attempt
              spans <- testkit.finishedSpans
            } yield {
              assertEquals(spans.map(_.attributes.elements), List(attributes))
              assertEquals(spans.map(_.events.elements), List(events))
              assertEquals(spans.map(_.status), List(status))
            }
          }
        }
    }
  }

  test("record cancellation during response body streaming") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          testkit.tracerProvider.get("tracer").flatMap { implicit tracer =>
            val body = fs2.Stream.eval(IO.never[Byte])

            val tracedServer = ServerMiddleware
              .default[IO]
              .buildHttpApp(HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok).withBodyStream(body))))

            val request = Request[IO](Method.GET, uri"http://localhost/")

            val status = StatusData(StatusCode.Error, "canceled")

            val attributes = Attributes(
              Attribute("http.request.method", "GET"),
              Attribute("url.path", "/"),
              Attribute("url.full", "http://localhost/"),
              Attribute("url.scheme", "http"),
              Attribute("server.address", "localhost"),
              Attribute("http.response.status_code", 200L),
            )

            for {
              res <- tracedServer.run(request)
              f <- res.body.compile.drain.start
              _ <- IO.sleep(10.millis)
              _ <- f.cancel
              spans <- testkit.finishedSpans
            } yield {
              assertEquals(spans.map(_.attributes.elements), List(attributes))
              assertEquals(spans.flatMap(_.events.elements), Nil)
              assertEquals(spans.map(_.status), List(status))
            }
          }
        }
    }
  }
}
