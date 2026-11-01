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

package org.http4s.otel4s.middleware.metrics.client

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.testkit.TestControl
import com.comcast.ip4s._
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Header
import org.http4s.HttpVersion
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.client.UriTemplateClassifier
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.InstrumentationScopeExpectation
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.MetricSpec
import org.typelevel.otel4s.semconv.Requirement
import org.typelevel.otel4s.semconv.SchemaUrls
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.attributes.UrlExperimentalAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ClientMetricsSemanticConventionsTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val duration = HttpMetrics.ClientRequestDuration
  private val active = HttpExperimentalMetrics.ClientActiveRequests
  private val requestSize = HttpExperimentalMetrics.ClientRequestBodySize
  private val responseSize = HttpExperimentalMetrics.ClientResponseBodySize
  private val request = Request[IO](uri = uri"https://example.test/items/123?token=secret#fragment")
  private val start = Attributes(
    HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )
  private val config =
    ClientMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize

  test("metric metadata and every point's required attributes match generated specifications") {
    exchange() { metrics =>
      check(metrics, start, completed())
      List(duration, active, requestSize, responseSize).foreach { spec =>
        val required = spec.attributeSpecs
          .filter(_.requirement.level == Requirement.Level.Required)
          .map(_.key)
          .toSet
        assertMetrics(
          metrics,
          metadata(spec, MetricExpectation.name(spec.name)).where(
            "every point contains the required semantic attributes"
          )(metric =>
            metric.data.points.forall(point => required.subsetOf(point.attributes.map(_.key).toSet))
          ),
        )
      }
    }
  }

  List(
    Method.CONNECT,
    Method.DELETE,
    Method.GET,
    Method.HEAD,
    Method.OPTIONS,
    Method.PATCH,
    Method.POST,
    Method.PUT,
    Method.QUERY,
    Method.TRACE,
  ).foreach { method =>
    test(s"http.request.method: known method $method") {
      exchange(req = request.withMethod(method)) { metrics =>
        check(metrics, start.added(HttpAttributes.HttpRequestMethod(method.name)), completed())
      }
    }
  }

  List("CUSTOM", "get").foreach { name =>
    test(
      s"http.request.method: unknown method $name becomes _OTHER without an original-method label"
    ) {
      exchange(req = request.withMethod(Method.fromString(name).toOption.get)) { metrics =>
        check(metrics, start.added(HttpAttributes.HttpRequestMethod("_OTHER")), completed())
      }
    }
  }

  List(
    (uri"http://example.test/items", Some("example.test"), Some(80L)),
    (uri"https://example.test:8443/items", Some("example.test"), Some(8443L)),
    (uri"http://192.0.2.1/items", Some("192.0.2.1"), Some(80L)),
    (uri"https://[2001:db8::1]/items", Some("2001:db8::1"), Some(443L)),
    (uri"custom://example.test/items", Some("example.test"), None),
    (uri"custom://example.test:9000/items", Some("example.test"), Some(9000L)),
    (uri"//example.test/items", Some("example.test"), None),
    (uri"/items", None, None),
  ).foreach { case (uri, address, port) =>
    test(s"server.address and server.port: $uri") {
      val labels = Attributes(HttpAttributes.HttpRequestMethod("GET")) ++
        address.map(ServerAttributes.ServerAddress(_)) ++ port.map(ServerAttributes.ServerPort(_))
      exchange(req = request.withUri(uri))(metrics => check(metrics, labels, completed()))
    }
  }

  test("server.port prefers connection information to the URI port and scheme default") {
    val req = request
      .withUri(uri"https://example.test:8443/items")
      .withAttribute(
        Request.Keys.ConnectionInfo,
        Request.Connection(
          SocketAddress(ip"127.0.0.1", port"12345"),
          SocketAddress(ip"192.0.2.1", port"9443"),
          secure = true,
        ),
      )
    exchange(req = req) { metrics =>
      check(metrics, start.added(ServerAttributes.ServerPort(9443L)), completed())
    }
  }

  List(
    (HttpVersion.`HTTP/1.0`, "1.0"),
    (HttpVersion.`HTTP/1.1`, "1.1"),
    (HttpVersion.`HTTP/2`, "2"),
    (HttpVersion.`HTTP/3`, "3"),
  ).foreach { case (version, expected) =>
    test(s"network.protocol.version: $version") {
      exchange(response = Response[IO](httpVersion = version)) { metrics =>
        check(metrics, start, completed(version = expected))
      }
    }
  }

  List(
    Status.Ok,
    Status.NoContent,
    Status.MovedPermanently,
    Status.BadRequest,
    Status.NotFound,
    Status.InternalServerError,
    Status.ServiceUnavailable,
  ).foreach { status =>
    test(s"http.response.status_code and conditional error.type: ${status.code}") {
      val error = Option.when(status.code >= 400)(ErrorAttributes.ErrorType(status.code.toString))
      exchange(response = Response[IO](status)) { metrics =>
        check(metrics, start, completed(status) ++ error)
      }
    }
  }

  List(
    ("suppress", ErrorClassifier.never, Status.ServiceUnavailable, None),
    (
      "include",
      ErrorClassifier.default.included(Status.MovedPermanently),
      Status.MovedPermanently,
      Some("301"),
    ),
  ).foreach { case (name, classifier, status, error) =>
    test(s"error.type follows the configured classifier: $name") {
      exchange(
        response = Response[IO](status),
        settings = config.withErrorClassifier(classifier),
      ) { metrics =>
        check(metrics, start, completed(status) ++ error.map(ErrorAttributes.ErrorType(_)))
      }
    }
  }

  List(
    ("default", config, request, start),
    ("opt-in", config.optIntoUrlScheme, request, start.added(UrlAttributes.UrlScheme("https"))),
    (
      "unavailable",
      config.optIntoUrlScheme,
      request.withUri(uri"/items"),
      Attributes(HttpAttributes.HttpRequestMethod("GET")),
    ),
  ).foreach { case (name, settings, req, labels) =>
    test(s"url.scheme: $name") {
      exchange(req = req, settings = settings)(metrics => check(metrics, labels, completed()))
    }
  }

  List(true, false).foreach { classified =>
    test(s"url.template: classified=$classified, without raw URL or path fallback") {
      val template = "/items/{id}"
      val settings = config.withUrlTemplateClassifier(_ => Option.when(classified)(template))
      val labels =
        start ++ Option.when(classified)(UrlExperimentalAttributes.UrlTemplate(template))
      exchange(settings = settings)(metrics => check(metrics, labels, completed()))
    }
  }

  test("url.template: templates aggregate requests and unknown paths have no raw-path fallback") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      var classifications = 0
      val classifier: UriTemplateClassifier = uri => {
        classifications += 1
        if (uri.path.renderString.startsWith("/items/")) Some("/items/{id}") else None
      }
      for {
        middleware <- ClientMetrics.middleware[IO](
          testkit,
          ClientMetricsConfig.default.optIntoActiveRequests.withUrlTemplateClassifier(classifier),
        )
        client = middleware(Client[IO](_ => Resource.pure(Response[IO]())))
        _ <- client.run(request).use(_.body.compile.drain)
        _ <- client
          .run(request.withUri(uri"https://example.test/items/456"))
          .use(_.body.compile.drain)
        _ <- client
          .run(request.withUri(uri"https://example.test/unknown/789"))
          .use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield {
        assertEquals(classifications, 3)
        val templated = start.added(Attribute("url.template", "/items/{id}"))
        assertMetrics(
          metrics,
          MetricExpectation
            .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
            .exactlyPoints(
              PointExpectation.numeric(0L).attributesExact(templated),
              PointExpectation.numeric(0L).attributesExact(start),
            ),
          MetricExpectation
            .histogram(HttpMetrics.ClientRequestDuration.name)
            .exactlyPoints(
              PointExpectation.histogram
                .count(2L)
                .attributesExact(templated ++ completed()),
              PointExpectation.histogram.count(1L).attributesExact(start ++ completed()),
            ),
        )
      }
    }
  }

  test("all supported attributes coexist, with supplied protocol name and custom labels") {
    val labels = start ++ Attributes(
      UrlAttributes.UrlScheme("https"),
      UrlExperimentalAttributes.UrlTemplate("/items/{id}"),
      NetworkAttributes.NetworkProtocolName("http"),
      Attribute("component", "client"),
      Attribute("operation", "fetch"),
    )
    val settings = config.optIntoUrlScheme
      .withUrlTemplateClassifier(_ => Some("/items/{id}"))
      .withAttributes(
        Attributes(Attribute("component", "client"), Attribute("operation", "shared"))
      )
      .withRequestAttributes(_ =>
        Attributes(
          Attribute("operation", "fetch"),
          NetworkAttributes.NetworkProtocolName("http"),
          HttpAttributes.HttpRequestMethod("override"),
          ServerAttributes.ServerAddress("override"),
          ServerAttributes.ServerPort(1L),
          UrlAttributes.UrlScheme("override"),
          UrlExperimentalAttributes.UrlTemplate("override"),
        )
      )
    exchange(
      response = Response[IO](Status.ServiceUnavailable, HttpVersion.`HTTP/2`),
      settings = settings,
    ) { metrics =>
      check(
        metrics,
        labels,
        completed(Status.ServiceUnavailable, "2")
          .added(ErrorAttributes.ErrorType("503")),
      )
      List(duration, active, requestSize, responseSize).foreach { spec =>
        val keys = spec.attributeSpecs.map(_.key).toSet
        assertMetrics(
          metrics,
          MetricExpectation
            .name(spec.name)
            .where(
              "every attribute listed in the generated metric specification is exercised"
            )(metric =>
              metric.data.points.forall(point => keys.subsetOf(point.attributes.map(_.key).toSet))
            ),
        )
      }
    }
  }

  List("acquisition-error", "body-error", "acquisition-cancel", "body-cancel").foreach { mode =>
    test(s"conditional response and error attributes: $mode") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
          val error = new IllegalStateException("failed")
          val acquired = mode.startsWith("body")
          val canceled = mode.endsWith("cancel")
          for {
            entered <- Deferred[IO, Unit]
            middleware <- ClientMetrics.middleware[IO](testkit, config)
            failure =
              if (canceled) entered.complete(()).void >> IO.never[Unit]
              else IO.raiseError[Unit](error)
            client = middleware(
              Client[IO](r =>
                Resource.eval(
                  r.body.compile.drain >>
                    (if (acquired)
                       IO.pure(
                         Response[IO](Status.ServiceUnavailable)
                           .putHeaders(Header.Raw(ci"Content-Length", "5"))
                           .withBodyStream(Stream.exec(failure))
                       )
                     else failure.as(Response[IO]()))
                )
              )
            )
            use = client
              .run(request.putHeaders(Header.Raw(ci"Content-Length", "3")))
              .use(_.body.compile.drain)
            _ <-
              if (canceled) for {
                fiber <- use.start
                _ <- entered.get
                _ <- fiber.cancel
              } yield ()
              else use.attempt.void
            metrics <- testkit.collectMetrics
          } yield {
            val response =
              if (acquired) completed(Status.ServiceUnavailable) else Attributes.empty
            val errorLabel =
              if (!canceled) Some(ErrorAttributes.ErrorType(error.getClass.getName))
              else if (acquired) Some(ErrorAttributes.ErrorType("503"))
              else None
            check(metrics, start, response ++ errorLabel, hasResponse = acquired)
          }
        }
      }
    }
  }

  test("each distinct metric point retains its own semantic attributes") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      val first = start.added(UrlAttributes.UrlScheme("https"))
      val second = first.added(HttpAttributes.HttpRequestMethod("POST"))
      for {
        middleware <- ClientMetrics.middleware[IO](testkit, config.optIntoUrlScheme)
        client = middleware(
          Client[IO](req =>
            Resource.pure(
              Response[IO](if (req.method == Method.POST) Status.ServiceUnavailable else Status.Ok)
                .putHeaders(Header.Raw(ci"Content-Length", "5"))
            )
          )
        )
        _ <- client
          .run(request.putHeaders(Header.Raw(ci"Content-Length", "3")))
          .use(_.body.compile.drain)
        _ <- client
          .run(
            request
              .withMethod(Method.POST)
              .putHeaders(Header.Raw(ci"Content-Length", "3"))
          )
          .use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield {
        assertMetrics(
          metrics,
          metadata(
            active,
            MetricExpectation
              .sum[Long](active.name)
              .monotonic(false)
              .exactlyPoints(
                PointExpectation.numeric(0L).attributesExact(first),
                PointExpectation.numeric(0L).attributesExact(second),
              ),
          ),
        )
        List(duration, requestSize, responseSize).foreach { spec =>
          assertMetrics(
            metrics,
            metadata(
              spec,
              MetricExpectation
                .histogram(spec.name)
                .exactlyPoints(
                  PointExpectation.histogram.count(1L).attributesExact(first ++ completed()),
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      second ++
                        completed(Status.ServiceUnavailable).added(ErrorAttributes.ErrorType("503"))
                    ),
                ),
            ),
          )
        }
      }
    }
  }

  test("active requests use the same start attributes at increment and decrement") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      val labels = start.added(UrlAttributes.UrlScheme("https"))
      for {
        during <- Ref.of[IO, List[MetricData]](Nil)
        middleware <- ClientMetrics.middleware[IO](testkit, config.optIntoUrlScheme)
        client = middleware(
          Client[IO](_ =>
            Resource.eval(
              testkit.collectMetrics
                .flatMap(during.set)
                .as(
                  Response[IO]()
                    .putHeaders(Header.Raw(ci"Content-Length", "5"))
                )
            )
          )
        )
        _ <- client
          .run(request.putHeaders(Header.Raw(ci"Content-Length", "3")))
          .use(_.body.compile.drain)
        inFlight <- during.get
        metrics <- testkit.collectMetrics
      } yield {
        assertMetrics(inFlight, activeRequests(1L, labels))
        check(metrics, labels, completed())
      }
    }
  }

  private def completed(status: Status = Status.Ok, version: String = "1.1"): Attributes =
    Attributes(
      HttpAttributes.HttpResponseStatusCode(status.code.toLong),
      NetworkAttributes.NetworkProtocolVersion(version),
    )

  private def metadata(spec: MetricSpec, expectation: MetricExpectation): MetricExpectation =
    expectation
      .unit(spec.unit)
      .description(spec.description)
      .scope(
        InstrumentationScopeExpectation
          .name("org.http4s.otel4s.middleware.client")
          .schemaUrl(SchemaUrls.V1_43_0)
      )

  private def histogram(spec: MetricSpec, attributes: Attributes): MetricExpectation =
    metadata(
      spec,
      MetricExpectation
        .histogram(spec.name)
        .exactlyPoints(
          PointExpectation.histogram.count(1L).attributesExact(attributes)
        ),
    )

  private def activeRequests(value: Long, attributes: Attributes): MetricExpectation =
    metadata(
      active,
      MetricExpectation
        .sum[Long](active.name)
        .monotonic(false)
        .exactlyPoints(
          PointExpectation.numeric(value).attributesExact(attributes)
        ),
    )

  private def check(
      metrics: List[MetricData],
      startAttributes: Attributes,
      completionAttributes: Attributes,
      hasResponse: Boolean = true,
  ): Unit = {
    val terminal = startAttributes ++ completionAttributes

    val expectations = List(
      activeRequests(0L, startAttributes),
      histogram(duration, terminal),
      histogram(requestSize, terminal),
    ) ++ Option.when(hasResponse)(histogram(responseSize, terminal))
    assertMetrics(metrics, expectations: _*)
    if (!hasResponse)
      assertEquals(
        MetricExpectations.find(metrics, MetricExpectation.name(responseSize.name)),
        None,
      )
  }

  private def exchange(
      req: Request[IO] = request,
      response: Response[IO] = Response[IO](),
      settings: ClientMetricsConfig = config,
  )(verify: List[MetricData] => Unit): IO[Unit] =
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      for {
        middleware <- ClientMetrics.middleware[IO](testkit, settings)
        client = middleware(
          Client[IO](r =>
            Resource.eval(
              r.body.compile.drain.as(
                response
                  .withBodyStream(Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO])
                  .putHeaders(Header.Raw(ci"Content-Length", "5"))
              )
            )
          )
        )
        _ <- client
          .run(req.putHeaders(Header.Raw(ci"Content-Length", "3")))
          .use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield verify(metrics)
    }

}
