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
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import scala.concurrent.duration.Duration
import scala.util.control.NoStackTrace

class ClientMiddlewareTest extends CatsEffectSuite {
  import ClientMiddlewareTest.MinimalRedactor

  private val spanLimits = SpanLimits.default

  test("composes middlewares") {
    def middleware(name: String)(implicit tracer: Tracer[IO]): ClientMiddleware[IO] =
      client =>
        Client[IO] { request =>
          for {
            spanOps <- tracer.span(name).resource
            response <- client.run(request).mapK(spanOps.trace)
          } yield response
        }

    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          tracer <- testkit.tracerProvider.get("test")
          _ <- {
            implicit val T: Tracer[IO] = tracer
            val clientMiddleware =
              middleware("outer").wrapMiddleware(middleware("inner"))
            val client = clientMiddleware.wrapClient {
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
              }
            }
            client
              .run(Request[IO](Method.GET, uri"http://localhost/"))
              .use(_.body.compile.drain)
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

  test("success with tracing enabled") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddleware
              .builder[IO] {
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
              }
              .build
          }
          _ <- {
            val headers =
              Headers(Header.Raw(ci"foo", "bar"), Header.Raw(ci"baz", "qux"))
            val response = Response[IO](Status.Ok).withHeaders(headers)
            val client = clientMiddleware.wrapClient {
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            }
            val request =
              Request[IO](Method.GET, uri"http://localhost/?#")
                .withHeaders(headers)
            client.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield assertSingleSpan(
          spans,
          SpanExpectation
            .client("GET")
            .status(StatusExpectation.unset)
            .attributesExact(
              Attribute("http.request.method", "GET"),
              Attribute("http.request.header.foo", Seq("bar")),
              Attribute("network.protocol.version", "1.1"),
              Attribute("server.address", "localhost"),
              Attribute("server.port", 80L),
              Attribute("url.full", "http://localhost/?#"),
              Attribute("url.scheme", "http"),
              Attribute("http.response.status_code", 200L),
              Attribute("http.response.header.baz", Seq("qux")),
              Attribute(
                "org.http4s.otel4s.middleware.version",
                org.http4s.otel4s.middleware.BuildInfo.version,
              ),
            ),
        )
      }
  }

  test("allows manipulating spans from inner clients") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddleware
              .builder[IO](
                ClientSpanDataProvider
                  .openTelemetry(MinimalRedactor)
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
                  .optIntoUrlScheme
                  .optIntoUserAgentOriginal
              )
              .build
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
            val tracedClient = clientMiddleware.wrapClient(traceManipulatingClient)
            val request = Request[IO](Method.GET, uri"http://localhost/?#")
            tracedClient.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield assertSingleSpan(spans, SpanExpectation.name("NEW SPAN NAME"))
      }
  }

  test("allows overriding span name") {
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
      override def errorAttributes(request: RequestPrelude, response: ResponsePrelude): Attributes =
        Attributes.empty
    }

    val spanName = "Overridden span name"
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddleware
              .builder[IO] {
                provider.and(
                  ClientSpanDataProvider
                    .openTelemetry(MinimalRedactor)
                    .optIntoHttpRequestHeaders(HeaderRedactor.default)
                    .optIntoHttpResponseHeaders(HeaderRedactor.default)
                    .optIntoUrlScheme
                    .optIntoUserAgentOriginal
                )
              }
              .build
          }
          _ <- {
            val response = Response[IO](Status.Ok)
            val client = clientMiddleware.wrapClient {
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(response))
              }
            }
            val request = Request[IO](Method.GET, uri"http://localhost/?#")
            client.run(request).use(_.body.compile.drain)
          }
          spans <- testkit.finishedSpans
        } yield assertSingleSpan(spans, SpanExpectation.name(spanName))
      }
  }

  test("records thrown exception from the client") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .build
            .flatMap { clientMiddleware =>
              val error = new RuntimeException("oops") with NoStackTrace {}
              val client = clientMiddleware.wrapClient {
                Client { (_: Request[IO]) =>
                  Resource.raiseError[IO, Response[IO], Throwable](error)
                }
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
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- client.run(request).use_.attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(spans, spanExpectation(attributes, status, events))
            }
        }
    }
  }

  test("records cancelation from the client") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client { (_: Request[IO]) =>
                  Resource.canceled[IO] >> Resource.never[IO, Response[IO]]
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/?#")
              val status = StatusData(StatusCode.Error, "canceled")

              for {
                f <- client.run(request).use_.start
                _ <- f.joinWithUnit
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(
                spans,
                SpanExpectation.any.status(statusExpectation(status)).eventCount(0),
              )
            }
        }
    }
  }

  test("records thrown exception from the request processing") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .build
            .flatMap { clientMiddleware =>
              val error = new RuntimeException("oops") with NoStackTrace {}
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
                }
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
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 200L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- client.run(request).surround(IO.raiseError(error)).attempt
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(spans, spanExpectation(attributes, status, events))
            }
        }
    }
  }

  test("records cancelation from the request processing") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/?#")
              val status = StatusData(StatusCode.Error, "canceled")

              for {
                f <- client.run(request).surround(IO.canceled).start
                _ <- f.joinWithUnit
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(
                spans,
                SpanExpectation.any.status(statusExpectation(status)).eventCount(0),
              )
            }
        }
    }
  }

  test("records error.type on error response (400-500)") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.InternalServerError)))
                }
              }
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
                _ <- client.run(request).use_
                spans <- testkit.finishedSpans
              } yield assertSingleSpan(spans, spanExpectation(attributes, status))
            }
        }
    }
  }

  test("propagates trace data into headers by default") {
    TracesTestkit
      .builder[IO]
      .addTracerProviderCustomizer(_.addTextMapPropagators(W3CTraceContextPropagator.default))
      .build
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddleware
              .builder[IO] {
                ClientSpanDataProvider
                  .openTelemetry(MinimalRedactor)
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
                  .optIntoUrlScheme
                  .optIntoUserAgentOriginal
              }
              .build
          }
          headers <- clientMiddleware
            .wrapClient {
              Client.fromHttpApp[IO] {
                HttpApp[IO] { request =>
                  request.body.compile.drain
                    .as(Response[IO](Status.Ok, headers = request.headers))
                }
              }
            }
            .run(Request[IO](Method.GET, uri"http://localhost/?#"))
            .use { response =>
              response.body.compile.drain.as(response.headers)
            }
          spans <- testkit.finishedSpans
        } yield assertSingleSpan(
          spans,
          SpanExpectation.any.where("traceparent header matches the client span") { span =>
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

  test("doesn't propagate when perRequestPropagationFilter returns Disabled") {
    TracesTestkit
      .builder[IO]
      .addTracerProviderCustomizer(_.addTextMapPropagators(W3CTraceContextPropagator.default))
      .build
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddleware
              .builder[IO] {
                ClientSpanDataProvider
                  .openTelemetry(MinimalRedactor)
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
                  .optIntoUrlScheme
                  .optIntoUserAgentOriginal
              }
              .withPerRequestPropagationFilter(PerRequestFilter.neverEnabled)
              .build
          }
          headers <- clientMiddleware
            .wrapClient {
              Client.fromHttpApp[IO] {
                HttpApp[IO] { request =>
                  request.body.compile.drain
                    .as(Response[IO](Status.Ok, headers = request.headers))
                }
              }
            }
            .run(Request[IO](Method.GET, uri"http://localhost/?#"))
            .use { response =>
              response.body.compile.drain.as(response.headers)
            }
          _ <- testkit.finishedSpans
        } yield assert(headers.get(ci"traceparent").isEmpty)
      }
  }

  test("doesn't trace when perRequestTracingFilter returns Disabled") {
    TracesTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          clientMiddleware <- {
            implicit val TP: TracerProvider[IO] = testkit.tracerProvider
            ClientMiddleware
              .builder[IO] {
                ClientSpanDataProvider
                  .openTelemetry(MinimalRedactor)
                  .optIntoHttpRequestHeaders(HeaderRedactor.default)
                  .optIntoHttpResponseHeaders(HeaderRedactor.default)
                  .optIntoUrlScheme
                  .optIntoUserAgentOriginal
              }
              .withPerRequestTracingFilter(PerRequestFilter.neverEnabled)
              .build
          }
          _ <- clientMiddleware
            .wrapClient {
              Client.fromHttpApp[IO] {
                HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
              }
            }
            .run(Request[IO](Method.GET, uri"http://localhost/?#"))
            .use(_.body.compile.drain)
          spans <- testkit.finishedSpans
        } yield assertTrace(spans, TraceForestExpectation.empty)
      }
  }

  test("treats 404 as an error by default") {
    TestControl.executeEmbed {
      TracesTestkit
        .inMemory[IO]()
        .use { testkit =>
          implicit val TP: TracerProvider[IO] = testkit.tracerProvider
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.NotFound)))
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")
              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                Attribute("error.type", "404"),
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 404L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- client.run(request).use_
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
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .withErrorClassifier(ErrorClassifier.default.excluding(Status.NotFound))
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.NotFound)))
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val attributes = Attributes(
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 404L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- client.run(request).use_
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
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .withErrorClassifier(ErrorClassifier.default.excluding(Status.NotFound))
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.InternalServerError)))
                }
              }
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
                _ <- client.run(request).use_
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
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .withErrorClassifier(ErrorClassifier.never.included(Status.Ok))
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")
              val status = StatusData(StatusCode.Error)

              val attributes = Attributes(
                Attribute("error.type", "200"),
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 200L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- client.run(request).use_
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
          ClientMiddleware
            .builder[IO] {
              ClientSpanDataProvider
                .openTelemetry(MinimalRedactor)
                .optIntoHttpRequestHeaders(HeaderRedactor.default)
                .optIntoHttpResponseHeaders(HeaderRedactor.default)
                .optIntoUrlScheme
                .optIntoUserAgentOriginal
            }
            .withErrorClassifier(ErrorClassifier.never)
            .build
            .flatMap { clientMiddleware =>
              val client = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.InternalServerError)))
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")

              val attributes = Attributes(
                Attribute("http.request.method", "GET"),
                Attribute("http.response.status_code", 500L),
                Attribute("network.protocol.version", "1.1"),
                Attribute("server.address", "localhost"),
                Attribute("server.port", 80L),
                Attribute("url.full", "http://localhost/"),
                Attribute("url.scheme", "http"),
              )

              for {
                _ <- client.run(request).use_
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
          ClientMiddleware
            .builder[IO](provider)
            .build
            .flatMap { clientMiddleware =>
              val errorClient = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.BadRequest)))
                }
              }
              val okClient = clientMiddleware.wrapClient {
                Client.fromHttpApp[IO] {
                  HttpApp[IO](_.body.compile.drain.as(Response[IO](Status.Ok)))
                }
              }
              val request = Request[IO](Method.GET, uri"http://localhost/")

              for {
                _ <- errorClient.run(request).use_
                _ <- okClient.run(request).use_
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
      .attributesExact(attributes)
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

object ClientMiddlewareTest {
  object MinimalRedactor extends UriRedactor.OnlyRedactUserInfo
}
