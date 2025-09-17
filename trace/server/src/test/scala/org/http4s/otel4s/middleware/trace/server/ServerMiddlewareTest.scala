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
package server

import cats.data.OptionT
import cats.effect.IO
import cats.effect.MonadCancelThrow
import cats.effect.testkit.TestControl
import munit.CatsEffectSuite
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.sdk.data.LimitedData
import org.typelevel.otel4s.sdk.testkit.trace.TracesTestkit
import org.typelevel.otel4s.sdk.trace.SpanLimits
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.sdk.trace.data.EventData
import org.typelevel.otel4s.sdk.trace.data.StatusData
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration.Duration
import scala.util.control.NoStackTrace

class ServerMiddlewareTest extends CatsEffectSuite {
  import ServerMiddlewareTest.NoopRedactor

  private val spanLimits = SpanLimits.default

  def suite(
      methodName: String
  )(wrapApp: (ServerMiddleware[IO], HttpApp[IO]) => HttpApp[IO]): Unit = {
    def wrap(serverMiddleware: ServerMiddleware[IO])(httpApp: HttpApp[IO]): HttpApp[IO] =
      wrapApp(serverMiddleware, httpApp)

    test(s"$methodName: composes middlewares") {
      def middleware(name: String)(implicit tracer: Tracer[IO]): ServerMiddleware[IO] =
        new ServerMiddleware[IO] {
          implicit def monadCancelThrow: MonadCancelThrow[IO] = IO.asyncForIO
          def wrapGenericHttp[G[_]: MonadCancelThrow](http: Http[G, IO])(implicit
              kt: KindTransformer[IO, G]
          ): Http[G, IO] = Http[G, IO] { request =>
            tracer.mapK[G].span(name).surround(http.run(request))
          }
        }

      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          for {
            tracer <- testkit.tracerProvider.get("test")
            _ <- {
              implicit val T: Tracer[IO] = tracer
              val serverMiddleware =
                middleware("outer").wrapMiddleware(middleware("inner"))
              val app = wrap(serverMiddleware) {
                HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
              }
              app.run(Request[IO](Method.GET, uri"http://localhost/"))
            }
            spans <- testkit.finishedSpans
          } yield {
            assertEquals(spans.length, 2)
            val spansByName = spans.groupMapReduce(_.name)(identity)((a, _) => a)
            // also checks that still size 2 and previous line didn't drop elements
            assertEquals(spansByName.keySet, Set("outer", "inner"))
            assertEquals(spansByName("outer").parentSpanContext, None)
            assertEquals(
              spansByName("inner").parentSpanContext,
              Some(spansByName("outer").spanContext),
            )
          }
        }
    }

    test(s"$methodName: success with tracing enabled") {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          val headers = Headers(Header.Raw(ci"foo", "bar"), Header.Raw(ci"baz", "qux"))
          val response = Response[IO](Status.Ok).withHeaders(headers)
          for {
            serverMiddleware <- {
              implicit val TP: TracerProvider[IO] = testkit.tracerProvider
              ServerMiddleware
                .builder[IO] {
                  ServerSpanDataProvider
                    .openTelemetry(NoopRedactor)
                    .optIntoClientPort
                    .optIntoHttpRequestHeaders(
                      HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.Elide)
                    )
                    .optIntoHttpResponseHeaders(
                      HeaderRedactor(Set(ci"baz"), HeaderRedactor.Behavior.Elide)
                    )
                    .and(AttributeProvider.middlewareVersion)
                }
                .build
            }
            _ <- {
              val app = wrap(serverMiddleware) {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
              val request =
                Request[IO](Method.GET, uri"http://localhost/?#")
                  .withHeaders(headers)
              app.run(request)
            }
            spans <- testkit.finishedSpans
          } yield {
            assertEquals(spans.length, 1)
            val span = spans.head
            assertEquals(span.name, "GET")
            assertEquals(span.kind, SpanKind.Server)
            assertEquals(span.status, StatusData.Unset)

            val attributes = span.attributes.elements
            assertEquals(attributes.size, 9)
            def getAttr[A: AttributeKey.KeySelect](name: String): Option[A] =
              attributes.get[A](name).map(_.value)

            assertEquals(getAttr[String]("http.request.method"), Some("GET"))
            assertEquals(getAttr[Seq[String]]("http.request.header.foo"), Some(Seq("bar")))
            assertEquals(getAttr[Seq[String]]("http.request.header.baz"), None)
            assertEquals(getAttr[String]("network.protocol.version"), Some("1.1"))
            assertEquals(getAttr[String]("url.scheme"), Some("http"))
            assertEquals(getAttr[String]("url.path"), Some("/"))
            assertEquals(getAttr[String]("url.query"), Some(""))
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

    test(s"$methodName: records an exception thrown by the server") {
      TestControl.executeEmbed {
        TracesTestkit
          .inMemory[IO]()
          .use { testkit =>
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            val error = new RuntimeException("oops") with NoStackTrace {}
            ServerMiddleware
              .builder[IO] {
                ServerSpanDataProvider
                  .openTelemetry(NoopRedactor)
                  .optIntoClientPort
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
              }
              .build
              .flatMap { serverMiddleware =>
                val app = wrap(serverMiddleware) {
                  HttpApp[IO](_ => IO.raiseError(error))
                }
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
                  )
                )

                val status = StatusData(StatusCode.Error)

                val attributes = Attributes(
                  Attribute("error.type", error.getClass.getName),
                  Attribute("http.request.method", "GET"),
                  Attribute("network.protocol.version", "1.1"),
                  Attribute("url.path", "/"),
                  Attribute("url.scheme", "http"),
                )

                for {
                  _ <- app.run(request).attempt
                  spans <- testkit.finishedSpans
                } yield {
                  assertEquals(spans.length, 1)
                  val span = spans.head
                  assertEquals(span.status, StatusData.Error(None))

                  assertEquals(spans.map(_.attributes.elements), List(attributes))
                  assertEquals(spans.map(_.events.elements), List(events))
                  assertEquals(spans.map(_.status), List(status))
                }
              }
          }
      }
    }

    test(s"$methodName: records error.type on error response 5xx") {
      TestControl.executeEmbed {
        TracesTestkit
          .inMemory[IO]()
          .use { testkit =>
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ServerMiddleware
              .builder[IO] {
                ServerSpanDataProvider
                  .openTelemetry(NoopRedactor)
                  .optIntoClientPort
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
              }
              .build
              .flatMap { serverMiddleware =>
                val app = wrap(serverMiddleware) {
                  HttpApp[IO](_ => IO.pure(Response[IO](Status.InternalServerError)))
                }
                val request = Request[IO](Method.GET, uri"http://localhost/")
                val status = StatusData(StatusCode.Error)

                val attributes = Attributes(
                  Attribute("error.type", "500"),
                  Attribute("http.request.method", "GET"),
                  Attribute("http.response.status_code", 500L),
                  Attribute("network.protocol.version", "1.1"),
                  Attribute("url.path", "/"),
                  Attribute("url.scheme", "http"),
                )

                for {
                  _ <- app.run(request).attempt
                  spans <- testkit.finishedSpans
                } yield {
                  assertEquals(spans.map(_.attributes.elements), List(attributes))
                  assertEquals(spans.map(_.status), List(status))
                }
              }
          }
      }
    }

    test(s"$methodName: records cancelation caused by the server") {
      TestControl.executeEmbed {
        TracesTestkit
          .inMemory[IO]()
          .use { testkit =>
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ServerMiddleware
              .builder[IO] {
                ServerSpanDataProvider
                  .openTelemetry(NoopRedactor)
                  .optIntoClientPort
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
              }
              .build
              .flatMap { serverMiddleware =>
                val app = wrap(serverMiddleware) {
                  HttpApp[IO](_ => IO.canceled.as(Response[IO](Status.Ok)))
                }
                val request = Request[IO](Method.GET, uri"http://localhost/")

                val status = StatusData(StatusCode.Error, "canceled")

                val attributes = Attributes(
                  Attribute("http.request.method", "GET"),
                  Attribute("network.protocol.version", "1.1"),
                  Attribute("url.path", "/"),
                  Attribute("url.scheme", "http"),
                )

                for {
                  f <- app.run(request).void.start
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

    test(s"$methodName: doesn't trace when PerRequestTracingFilter returns Disabled") {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          val response = Response[IO](Status.Ok)
          for {
            serverMiddleware <- {
              implicit val TP: TracerProvider[IO] = testkit.tracerProvider
              ServerMiddleware
                .builder[IO] {
                  ServerSpanDataProvider
                    .openTelemetry(NoopRedactor)
                    .optIntoClientPort
                    .optIntoHttpRequestHeaders(HeaderRedactor.default)
                    .optIntoHttpResponseHeaders(HeaderRedactor.default)
                }
                .withPerRequestTracingFilter(PerRequestFilter.neverEnabled)
                .build
            }
            _ <- wrap(serverMiddleware) {
              HttpApp[IO](_.body.compile.drain.as(response))
            }
              .run(Request[IO](Method.GET, uri"http://localhost/?#"))
            spans <- testkit.finishedSpans
          } yield assertEquals(spans.length, 0)
        }
    }

    test(s"$methodName: doesn't propagate trace data to requesting client by default") {
      TracesTestkit
        .inMemory[IO](_.addTextMapPropagators(W3CTraceContextPropagator.default))
        .use { testkit =>
          for {
            serverMiddleware <- {
              implicit val TP: TracerProvider[IO] = testkit.tracerProvider
              ServerMiddleware
                .builder[IO] {
                  ServerSpanDataProvider
                    .openTelemetry(NoopRedactor)
                    .optIntoClientPort
                    .optIntoHttpRequestHeaders(HeaderRedactor.default)
                    .optIntoHttpResponseHeaders(HeaderRedactor.default)
                }
                .build
            }
            headers <- wrap(serverMiddleware) {
              HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
            }
              .run(Request[IO](Method.GET, uri"http://localhost/"))
              .map(_.headers)
            _ <- testkit.finishedSpans
          } yield assert(headers.get(ci"traceparent").isEmpty)
        }
    }

    test(
      s"$methodName: propagates trace data to requesting client when perRequestReversePropagationFilter returns Enabled"
    ) {
      TracesTestkit
        .inMemory[IO](_.addTextMapPropagators(W3CTraceContextPropagator.default))
        .use { testkit =>
          for {
            serverMiddleware <- {
              implicit val TP: TracerProvider[IO] = testkit.tracerProvider
              ServerMiddleware
                .builder[IO] {
                  ServerSpanDataProvider
                    .openTelemetry(NoopRedactor)
                    .optIntoClientPort
                    .optIntoHttpRequestHeaders(HeaderRedactor.default)
                    .optIntoHttpResponseHeaders(HeaderRedactor.default)
                }
                .withPerRequestReversePropagationFilter(PerRequestFilter.alwaysEnabled)
                .build
            }
            headers <- wrap(serverMiddleware) {
              HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
            }
              .run(Request[IO](Method.GET, uri"http://localhost/"))
              .map(_.headers)
            spans <- testkit.finishedSpans
          } yield {
            assertEquals(spans.length, 1)
            val spanCtx = spans.head.spanContext
            val traceparentHeader = headers.get(ci"traceparent")
            assert(traceparentHeader.isDefined)
            assertEquals(traceparentHeader.get.length, 1)
            assertEquals(
              traceparentHeader.get.head.value,
              s"00-${spanCtx.traceIdHex}-${spanCtx.spanIdHex}-${spanCtx.traceFlags.toHex}",
            )
          }
        }
    }
  }

  suite("wrapGenericHttp")(_.wrapGenericHttp(_))
  suite("asGenericHttpMiddleware")(_.asGenericHttpMiddleware[IO].apply(_))
  suite("wrapHttpApp")(_.wrapHttpApp(_))
  suite("asHttpAppMiddleware")(_.asHttpAppMiddleware(_))
  suite("wrapHttpRoutes") { (sm, app) =>
    sm.wrapHttpRoutes(app.mapK(OptionT.liftK)).orNotFound
  }
  suite("asHttpRoutesMiddleware") { (sm, app) =>
    sm.asHttpRoutesMiddleware(app.mapK(OptionT.liftK)).orNotFound
  }
}

object ServerMiddlewareTest {
  object NoopRedactor extends PathRedactor.NeverRedact with QueryRedactor.NeverRedact
}
