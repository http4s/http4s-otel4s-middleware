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
package otel4s.middleware.trace
package client

import cats.effect.IO
import cats.effect.Resource
import cats.effect.testkit.TestControl
import cats.syntax.flatMap._
import munit.CatsEffectSuite
import org.http4s.client.Client
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
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
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration.Duration
import scala.util.control.NoStackTrace

class ClientMiddlewareTest extends CatsEffectSuite {
  import ClientMiddlewareTest.MinimalRedactor

  private val spanLimits = SpanLimits.default

  test("ClientMiddleware") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddlewareBuilder(
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(
                  HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.Elide)
                )
                .optIntoHttpResponseHeaders(
                  HeaderRedactor(Set(ci"baz"), HeaderRedactor.Behavior.Elide)
                )
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
                .and(AttributeProvider.middlewareVersion)
            ).build
          }
          _ <- {
            val headers =
              Headers(Header.Raw(ci"foo", "bar"), Header.Raw(ci"baz", "qux"))
            val response = Response[IO](Status.Ok).withHeaders(headers)
            val fakeClient =
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            val tracedClient = clientMiddleware(fakeClient)
            val request =
              Request[IO](Method.GET, uri"http://localhost/?#")
                .withHeaders(headers)
            tracedClient.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield {
          assertEquals(spans.length, 1)
          val span = spans.head
          assertEquals(span.name, "GET")
          assertEquals(span.kind, SpanKind.Client)
          assertEquals(span.status, StatusData.Unset)

          val attributes = span.attributes.elements
          assertEquals(attributes.size, 10)
          def getAttr[A: AttributeKey.KeySelect](name: String): Option[A] =
            attributes.get[A](name).map(_.value)

          assertEquals(getAttr[String]("http.request.method"), Some("GET"))
          assertEquals(getAttr[Seq[String]]("http.request.header.foo"), Some(Seq("bar")))
          assertEquals(getAttr[Seq[String]]("http.request.header.baz"), None)
          assertEquals(getAttr[String]("network.protocol.version"), Some("1.1"))
          assertEquals(getAttr[String]("server.address"), Some("localhost"))
          assertEquals(getAttr[Long]("server.port"), Some(80L))
          assertEquals(getAttr[String]("url.full"), Some("http://localhost/?#"))
          assertEquals(getAttr[String]("url.scheme"), Some("http"))
          assertEquals(getAttr[Long]("http.response.status_code"), Some(200L))
          assertEquals(getAttr[Seq[String]]("http.response.header.foo"), None)
          assertEquals(getAttr[Seq[String]]("http.response.header.baz"), Some(Seq("qux")))
          assertEquals(
            getAttr[String]("org.http4s.otel4s.middleware.version"),
            Some(org.http4s.otel4s.middleware.BuildInfo.version),
          )
        }
      }
  }

  test("ClientMiddleware allows manipulating spans from inner clients") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddlewareBuilder(
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            ).build
          }
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
            val tracedClient = clientMiddleware(traceManipulatingClient)
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
    val provider: SpanDataProvider = new SpanDataProvider {
      type Shared = Null
      def processSharedData[F[_]](request: Request[F]): Null = null
      def spanName[F[_]](request: Request[F], sharedProcessedData: Null): String =
        "Overridden span name"
      def requestAttributes[F[_]](request: Request[F], sharedProcessedData: Null): Attributes =
        Attributes.empty
      def responseAttributes[F[_]](response: Response[F]): Attributes =
        Attributes.empty
      def exceptionAttributes(cause: Throwable): Attributes =
        Attributes.empty
    }

    val spanName = "Overridden span name"
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddlewareBuilder(
              provider.and(
                ClientSpanDataProvider
                  .openTelemetry(MinimalRedactor)
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
                  .optIntoUrlScheme
                  .optIntoUserAgentOriginal
              )
            ).build
          }
          _ <- {
            val response = Response[IO](Status.Ok)
            val fakeClient =
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            val tracedClient = clientMiddleware(fakeClient)
            val request = Request[IO](Method.GET, uri"http://localhost/?#")
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

  test("record thrown exception from the client") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddlewareBuilder(
            ClientSpanDataProvider
              .openTelemetry(MinimalRedactor)
              .optIntoHttpRequestHeaders(HeaderRedactor.default)
              .optIntoHttpResponseHeaders(HeaderRedactor.default)
              .optIntoUrlScheme
              .optIntoUserAgentOriginal
          ).build
            .flatMap { clientMiddleware =>
              val error = new RuntimeException("oops") with NoStackTrace {}

              val fakeClient = Client { (_: Request[IO]) =>
                Resource.raiseError[IO, Response[IO], Throwable](error)
              }

              val tracedClient = clientMiddleware(fakeClient)
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val events = Vector(
                EventData.fromException(
                  Duration.Zero,
                  error,
                  LimitedData
                    .attributes(
                      spanLimits.maxNumberOfAttributes,
                      spanLimits.maxAttributeValueLength,
                    ),
                  escaped = false,
                )
              )

              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                Attribute("error.type", error.getClass.getName),
                Attribute("http.request.method", "GET"),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- tracedClient.run(request).use_.attempt
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

  test("record cancelation from the client") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddlewareBuilder(
            ClientSpanDataProvider
              .openTelemetry(MinimalRedactor)
              .optIntoHttpRequestHeaders(HeaderRedactor.default)
              .optIntoHttpResponseHeaders(HeaderRedactor.default)
              .optIntoUrlScheme
              .optIntoUserAgentOriginal
          ).build
            .flatMap { clientMiddleware =>
              val fakeClient = Client { (_: Request[IO]) =>
                Resource.canceled[IO] >> Resource.never[IO, Response[IO]]
              }

              val tracedClient = clientMiddleware(fakeClient)
              val request = Request[IO](Method.GET, uri"http://localhost/?#")

              val status = StatusData(StatusCode.Error, "canceled")

              for {
                f <- tracedClient.run(request).use_.start
                _ <- f.joinWithUnit
                spans <- testkit.finishedSpans
              } yield {
                assertEquals(spans.flatMap(_.events.elements), Nil)
                assertEquals(spans.map(_.status), List(status))
              }
            }
        }
    }
  }

  test("record thrown exception from the request processing") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddlewareBuilder(
            ClientSpanDataProvider
              .openTelemetry(MinimalRedactor)
              .optIntoHttpRequestHeaders(HeaderRedactor.default)
              .optIntoHttpResponseHeaders(HeaderRedactor.default)
              .optIntoUrlScheme
              .optIntoUserAgentOriginal
          ).build
            .flatMap { clientMiddleware =>
              val error = new RuntimeException("oops") with NoStackTrace {}

              val fakeClient =
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
                }

              val tracedClient = clientMiddleware(fakeClient)
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val events = Vector(
                EventData.fromException(
                  Duration.Zero,
                  error,
                  LimitedData
                    .attributes(
                      spanLimits.maxNumberOfAttributes,
                      spanLimits.maxAttributeValueLength,
                    ),
                  escaped = false,
                )
              )

              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 200L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- tracedClient.run(request).surround(IO.raiseError(error)).attempt
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

  test("record cancelation from the request processing") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddlewareBuilder(
            ClientSpanDataProvider
              .openTelemetry(MinimalRedactor)
              .optIntoHttpRequestHeaders(HeaderRedactor.default)
              .optIntoHttpResponseHeaders(HeaderRedactor.default)
              .optIntoUrlScheme
              .optIntoUserAgentOriginal
          ).build
            .flatMap { clientMiddleware =>
              val fakeClient =
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
                }

              val tracedClient = clientMiddleware(fakeClient)
              val request = Request[IO](Method.GET, uri"http://localhost/?#")

              val status = StatusData(StatusCode.Error, "canceled")

              for {
                f <- tracedClient.run(request).surround(IO.canceled).start
                _ <- f.joinWithUnit
                spans <- testkit.finishedSpans
              } yield {
                assertEquals(spans.flatMap(_.events.elements), Nil)
                assertEquals(spans.map(_.status), List(status))
              }
            }
        }
    }
  }

  test("record error.type on error response (400-500)") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddlewareBuilder(
            ClientSpanDataProvider
              .openTelemetry(MinimalRedactor)
              .optIntoHttpRequestHeaders(HeaderRedactor.default)
              .optIntoHttpResponseHeaders(HeaderRedactor.default)
              .optIntoUrlScheme
              .optIntoUserAgentOriginal
          ).build
            .flatMap { clientMiddleware =>
              val fakeClient =
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.InternalServerError)))
                }

              val tracedClient = clientMiddleware(fakeClient)
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                Attribute("error.type", "500"),
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 500L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- tracedClient.run(request).use_
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

  test("don't trace when PerRequestTracingFilter returns Disabled") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddlewareBuilder(
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            )
              .withPerRequestTracingFilter(PerRequestTracingFilter.neverTrace)
              .build
          }
          _ <- {
            val fakeClient =
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
              }
            clientMiddleware(fakeClient)
              .run(Request[IO](Method.GET, uri"http://localhost/?#"))
              .use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield assertEquals(spans.length, 0)
      }
  }
}

object ClientMiddlewareTest {
  object MinimalRedactor extends UriRedactor.OnlyRedactUserInfo
}
