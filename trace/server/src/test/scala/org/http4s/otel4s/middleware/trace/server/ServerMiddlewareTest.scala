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
import cats.mtl.LiftKind
import munit.CatsEffectSuite
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.data.LimitedData
import org.typelevel.otel4s.sdk.testkit.trace.EventExpectation
import org.typelevel.otel4s.sdk.testkit.trace.EventSetExpectation
import org.typelevel.otel4s.sdk.testkit.trace.SpanExpectation
import org.typelevel.otel4s.sdk.testkit.trace.StatusExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TraceExpectations
import org.typelevel.otel4s.sdk.testkit.trace.TraceForestExpectation
import org.typelevel.otel4s.sdk.testkit.trace.TracesTestkit
import org.typelevel.otel4s.sdk.trace.SpanLimits
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator
import org.typelevel.otel4s.sdk.trace.data.EventData
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.sdk.trace.data.StatusData
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration.Duration
import scala.util.control.NoStackTrace

class ServerMiddlewareTest extends CatsEffectSuite {
  import ServerMiddlewareTest.{NoopRedactor, ServerEndpointAttributes}

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
              kt: LiftKind[IO, G]
          ): Http[G, IO] = Http[G, IO] { request =>
            tracer.liftTo[G].span(name).surround(http.run(request))
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
          } yield assertTrace(
            spans,
            TraceForestExpectation.unordered(
              TraceExpectation.unordered(
                SpanExpectation.name("outer").noParentSpanContext,
                TraceExpectation.leaf(SpanExpectation.name("inner")),
              )
            ),
          )
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
          } yield assertSingleSpan(
            spans,
            SpanExpectation
              .server("GET")
              .status(StatusExpectation.unset)
              .attributesExact(
                HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                HttpAttributes.HttpRequestHeader.transformName(_ + ".foo")(Seq("bar")),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                ServerAttributes.ServerAddress("localhost"),
                ServerAttributes.ServerPort(80L),
                UrlAttributes.UrlScheme("http"),
                UrlAttributes.UrlPath("/"),
                UrlAttributes.UrlQuery(""),
                HttpAttributes.HttpResponseStatusCode(200L),
                HttpAttributes.HttpResponseHeader.transformName(_ + ".baz")(Seq("qux")),
                Attribute(
                  "org.http4s.otel4s.middleware.version",
                  org.http4s.otel4s.middleware.BuildInfo.version,
                ),
              ),
          )
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
                  ErrorAttributes.ErrorType(error.getClass.getName),
                  HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                  NetworkAttributes.NetworkProtocolVersion("1.1"),
                  UrlAttributes.UrlPath("/"),
                  UrlAttributes.UrlScheme("http"),
                )

                for {
                  _ <- app.run(request).attempt
                  spans <- testkit.finishedSpans
                } yield assertSingleSpan(spans, spanExpectation(attributes, status, events))
              }
          }
      }
    }
    test(s"$methodName: does not set error status on a client error response") {
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
                  HttpApp[IO](_ => IO.pure(Response[IO](Status.BadRequest)))
                }
                val request = Request[IO](Method.GET, uri"http://localhost/")
                val status = StatusData(StatusCode.Unset)

                val attributes = Attributes(
                  HttpAttributes.HttpResponseStatusCode(Status.BadRequest.code.longValue),
                  HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                  NetworkAttributes.NetworkProtocolVersion("1.1"),
                  UrlAttributes.UrlPath("/"),
                  UrlAttributes.UrlScheme("http"),
                )

                for {
                  _ <- app.run(request).attempt
                  spans <- testkit.finishedSpans
                } yield assertSingleSpan(spans, spanExpectation(attributes, status))
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
                  ErrorAttributes.ErrorType("500"),
                  HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                  HttpAttributes.HttpResponseStatusCode(500L),
                  NetworkAttributes.NetworkProtocolVersion("1.1"),
                  UrlAttributes.UrlPath("/"),
                  UrlAttributes.UrlScheme("http"),
                )

                for {
                  _ <- app.run(request).attempt
                  spans <- testkit.finishedSpans
                } yield assertSingleSpan(
                  spans,
                  SpanExpectation.any
                    .attributesExact(attributes.concat(ServerEndpointAttributes))
                    .status(statusExpectation(status)),
                )
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
                  HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                  NetworkAttributes.NetworkProtocolVersion("1.1"),
                  UrlAttributes.UrlPath("/"),
                  UrlAttributes.UrlScheme("http"),
                )

                for {
                  f <- app.run(request).void.start
                  _ <- f.joinWithUnit
                  spans <- testkit.finishedSpans
                } yield assertSingleSpan(spans, spanExpectation(attributes, status))
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
          } yield assertTrace(spans, TraceForestExpectation.empty)
        }
    }

    test(s"$methodName: doesn't propagate trace data to requesting client by default") {
      TracesTestkit
        .builder[IO]
        .addTracerProviderCustomizer(_.addTextMapPropagators(W3CTraceContextPropagator.default))
        .build
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
        .builder[IO]
        .addTracerProviderCustomizer(_.addTextMapPropagators(W3CTraceContextPropagator.default))
        .build
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
          } yield assertSingleSpan(
            spans,
            SpanExpectation.any.where("traceparent header matches the server span") { span =>
              headers
                .get(ci"traceparent")
                .exists(
                  _.map(_.value).toList == List(
                    s"00-${span.spanContext.traceIdHex}-${span.spanContext.spanIdHex}-${span.spanContext.traceFlags.toHex}"
                  )
                )
            },
          )
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

  test("treats 500 as an error by default") {
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
              val app = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.InternalServerError)))
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")
              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                ErrorAttributes.ErrorType("500"),
                HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                HttpAttributes.HttpResponseStatusCode(500L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                UrlAttributes.UrlPath("/"),
                UrlAttributes.UrlScheme("http"),
              )

              for {
                _ <- app.run(request).attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(spans, spanExpectation(attributes, status))
            }
        }
    }
  }

  test("does not treat a status excluded by the error classifier as an error") {
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
            .withErrorClassifier(ErrorClassifier.serverError.excluding(Status.InternalServerError))
            .build
            .flatMap { serverMiddleware =>
              val app = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.InternalServerError)))
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val attributes = Attributes(
                HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                HttpAttributes.HttpResponseStatusCode(500L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                UrlAttributes.UrlPath("/"),
                UrlAttributes.UrlScheme("http"),
              )

              for {
                _ <- app.run(request).attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(
                spans,
                spanExpectation(attributes, StatusData.Unset),
              )
            }
        }
    }
  }

  test("still treats other error statuses as errors when on is excluded") {
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
            .withErrorClassifier(ErrorClassifier.serverError.excluding(Status.InternalServerError))
            .build
            .flatMap { serverMiddleware =>
              val app = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.ServiceUnavailable)))
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")
              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                ErrorAttributes.ErrorType("503"),
                HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                HttpAttributes.HttpResponseStatusCode(503L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                UrlAttributes.UrlPath("/"),
                UrlAttributes.UrlScheme("http"),
              )

              for {
                _ <- app.run(request).attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(spans, spanExpectation(attributes, status))
            }
        }
    }
  }

  test("records error.type for a status the error classifier add") {
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
            .withErrorClassifier(ErrorClassifier.never.included(Status.Ok))
            .build
            .flatMap { serverMiddleware =>
              val app = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok)))
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")
              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                ErrorAttributes.ErrorType("200"),
                HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                HttpAttributes.HttpResponseStatusCode(200L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                UrlAttributes.UrlPath("/"),
                UrlAttributes.UrlScheme("http"),
              )

              for {
                _ <- app.run(request).attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(spans, spanExpectation(attributes, status))
            }
        }
    }
  }

  test("never reports response statuses as errros with ErrorClassifier.never") {
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
            .withErrorClassifier(ErrorClassifier.never)
            .build
            .flatMap { serverMiddleware =>
              val app = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.InternalServerError)))
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val attributes = Attributes(
                HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                HttpAttributes.HttpResponseStatusCode(500L),
                NetworkAttributes.NetworkProtocolVersion("1.1"),
                UrlAttributes.UrlPath("/"),
                UrlAttributes.UrlScheme("http"),
              )

              for {
                _ <- app.run(request).attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(
                spans,
                spanExpectation(attributes, StatusData.Unset),
              )
            }
        }
    }
  }

  test("uses errorAttributes from the SpanDataProvider only for errors") {
    val customErrorAttr = Attribute("custom.error.attribute", "error-value")
    val provider: SpanDataProvider = new SpanDataProvider {
      type Shared = Null
      def processSharedData[F[_]](request: Request[F]): Null = null
      def spanName[F[_]](request: Request[F], sharedProcessedData: Null): String =
        "test-span"
      def requestAttributes[F[_]](request: Request[F], sharedProcessedData: Null): Attributes =
        Attributes.empty
      def responseAttributes[F[_]](response: Response[F]): Attributes =
        Attributes.empty
      def exceptionAttributes(cause: Throwable): Attributes =
        Attributes.empty
      override def errorAttributes(request: RequestPrelude, response: ResponsePrelude): Attributes =
        Attributes(customErrorAttr)
    }

    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ServerMiddleware
            .builder[IO](provider)
            .build
            .flatMap { serverMiddleware =>
              val errorApp = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.InternalServerError)))
              }
              val okApp = serverMiddleware.wrapHttpApp {
                HttpApp[IO](_ => IO.pure(Response[IO](Status.Ok)))
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")

              for {
                _ <- errorApp.run(request).attempt
                _ <- okApp.run(request).attempt
                spans <- testkit.finishedSpans
              } yield assertTrace(
                spans,
                TraceForestExpectation.unordered(
                  TraceExpectation.leaf(
                    SpanExpectation
                      .name("test-span")
                      .status(StatusExpectation.error)
                      .attributesSubset(customErrorAttr)
                  ),
                  TraceExpectation.leaf(
                    SpanExpectation
                      .name("test-span")
                      .status(StatusExpectation.unset)
                      .attributesEmpty
                  ),
                ),
              )
            }
        }
    }
  }

  private def eventExpectation(event: EventData): EventExpectation =
    EventExpectation
      .name(event.name)
      .timestamp(event.timestamp)
      .attributesExact(event.attributes.elements)

  private def eventsExpectation(events: Vector[EventData]): EventSetExpectation =
    events.headOption.fold(EventSetExpectation.count(0))(head =>
      EventSetExpectation.exactly(eventExpectation(head), events.tail.map(eventExpectation): _*)
    )

  private def statusExpectation(status: StatusData): StatusExpectation =
    StatusExpectation.code(status.status).description(status.description)

  private def spanExpectation(
      attributes: Attributes,
      status: StatusData,
      events: Vector[EventData] = Vector.empty,
  ): SpanExpectation =
    SpanExpectation.any
      .attributesExact(attributes.concat(ServerEndpointAttributes))
      .status(statusExpectation(status))
      .events(eventsExpectation(events))

  private def assertTrace(spans: List[SpanData], expectation: TraceForestExpectation): Unit =
    TraceExpectations
      .check(spans, expectation)
      .fold(
        mismatches => fail(TraceExpectations.format(mismatches)),
        identity,
      )

  private def assertSingleSpan(spans: List[SpanData], expectation: SpanExpectation): Unit =
    assertTrace(
      spans,
      TraceForestExpectation.unordered(TraceExpectation.leaf(expectation)),
    )
}

object ServerMiddlewareTest {
  object NoopRedactor extends PathRedactor.NeverRedact with QueryRedactor.NeverRedact

  private val ServerEndpointAttributes = Attributes(
    ServerAttributes.ServerAddress("localhost"),
    ServerAttributes.ServerPort(80L),
  )
}
