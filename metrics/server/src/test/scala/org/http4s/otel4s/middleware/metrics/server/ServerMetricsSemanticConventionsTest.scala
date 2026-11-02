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

package org.http4s.otel4s.middleware.metrics.server

import cats.effect.IO
import com.comcast.ip4s._
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.HttpApp
import org.http4s.HttpVersion
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.InstrumentationScopeExpectation
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.MetricSpec
import org.typelevel.otel4s.semconv.Requirement
import org.typelevel.otel4s.semconv.SchemaUrls
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.attributes.UserAgentExperimentalAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class ServerMetricsSemanticConventionsTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val duration = HttpMetrics.ServerRequestDuration
  private val active = HttpExperimentalMetrics.ServerActiveRequests
  private val requestSize = HttpExperimentalMetrics.ServerRequestBodySize
  private val responseSize = HttpExperimentalMetrics.ServerResponseBodySize
  private val request = Request[IO](uri = uri"https://example.test/items/123?secret=value")
    .putHeaders(Header.Raw(ci"Host", "example.test"))
  private val start =
    Attributes(HttpAttributes.HttpRequestMethod("GET"), UrlAttributes.UrlScheme("https"))
  private val config =
    ServerMetricsConfig.default.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize

  test("generated specifications define metadata and required attributes on every point") {
    exchange() { metrics =>
      check(metrics, start, terminal())
      List(duration, active, requestSize, responseSize).foreach { spec =>
        val keys = spec.attributeSpecs
          .filter(_.requirement.level == Requirement.Level.Required)
          .map(_.key)
          .toSet
        assertMetrics(
          metrics,
          MetricExpectation
            .name(spec.name)
            .where("required attributes on every point")(metric =>
              metric.data.points.forall(point => keys.subsetOf(point.attributes.map(_.key).toSet))
            ),
        )
      }
    }
  }

  List(
    Method.GET,
    Method.QUERY,
  ).foreach { method =>
    test(s"http.request.method: $method") {
      exchange(req = request.withMethod(method))(metrics =>
        check(
          metrics,
          start.added(HttpAttributes.HttpRequestMethod(method.name)),
          terminal(),
        )
      )
    }
  }
  List("CUSTOM", "get").foreach { method =>
    test(s"unknown method $method uses _OTHER without an original-method label") {
      exchange(req = request.withMethod(Method.fromString(method).toOption.get))(metrics =>
        check(metrics, start.added(HttpAttributes.HttpRequestMethod("_OTHER")), terminal())
      )
    }
  }

  List(
    Status.Ok,
    Status.MovedPermanently,
    Status.NotFound,
    Status.ServiceUnavailable,
  ).foreach { status =>
    test(s"response status ${status.code}: only 5xx are errors by default") {
      exchange(status = status)(metrics =>
        check(
          metrics,
          start,
          terminal(status) ++
            Option.when(status.code >= 500)(ErrorAttributes.ErrorType(status.code.toString)),
        )
      )
    }
  }
  List(
    (HttpVersion.`HTTP/1.0`, "1.0"),
    (HttpVersion.`HTTP/1.1`, "1.1"),
    (HttpVersion.`HTTP/2`, "2"),
    (HttpVersion.`HTTP/3`, "3"),
  ).foreach { case (version, expected) =>
    test(s"protocol version uses the request's negotiated version: $version") {
      exchange(req = request.withHttpVersion(version))(metrics =>
        check(metrics, start, terminal(version = expected))
      )
    }
  }

  List(true, false).foreach { secure =>
    test(s"relative-URI scheme comes from the transport: secure=$secure") {
      val req = request
        .withUri(uri"/items/123")
        .withAttribute(
          Request.Keys.ConnectionInfo,
          Request.Connection(
            SocketAddress(ip"127.0.0.1", port"8080"),
            SocketAddress(ip"192.0.2.1", port"12345"),
            secure,
          ),
        )
      exchange(req = req)(metrics =>
        check(
          metrics,
          start.added(UrlAttributes.UrlScheme(if (secure) "https" else "http")),
          terminal(),
        )
      )
    }
  }
  test("an unavailable scheme is omitted rather than guessed") {
    exchange(req = request.withUri(uri"/items/123"))(metrics =>
      check(metrics, Attributes(HttpAttributes.HttpRequestMethod("GET")), terminal())
    )
  }

  List(
    ("host", request, "example.test", 443L, "https"),
    (
      "host port",
      request.putHeaders(Header.Raw(ci"Host", "example.test:8443")),
      "example.test",
      8443L,
      "https",
    ),
    (
      "forwarded",
      request.putHeaders(Header.Raw(ci"Forwarded", "host=\"public.test:9443\";proto=http")),
      "public.test",
      9443L,
      "http",
    ),
    (
      "x-forwarded",
      request.putHeaders(
        Header.Raw(ci"X-Forwarded-Host", "public.test:9000"),
        Header.Raw(ci"X-Forwarded-Proto", "http"),
      ),
      "public.test",
      9000L,
      "http",
    ),
    (
      "authority",
      request.withHttpVersion(HttpVersion.`HTTP/2`).withUri(uri"https://authority.test:7443/items"),
      "authority.test",
      7443L,
      "https",
    ),
  ).foreach { case (name, req, address, port, scheme) =>
    test(s"server address/port extraction: $name") {
      val settings = config.optIntoServerAddress.optIntoServerPort
      val labels = start.added(UrlAttributes.UrlScheme(scheme)) ++ Attributes(
        ServerAttributes.ServerAddress(address),
        ServerAttributes.ServerPort(port),
      )
      exchange(req = req, settings = settings)(metrics =>
        check(
          metrics,
          labels,
          terminal(version = if (req.httpVersion == HttpVersion.`HTTP/2`) "2" else "1.1"),
        )
      )
    }
  }
  List((true, false), (false, true)).foreach { case (withAddress, withPort) =>
    test(s"server address and port opt in independently: address=$withAddress port=$withPort") {
      val settings = if (withAddress) config.optIntoServerAddress else config.optIntoServerPort
      val labels = start ++
        Option.when(withAddress)(ServerAttributes.ServerAddress("example.test")) ++
        Option.when(withPort)(ServerAttributes.ServerPort(443L))
      exchange(settings = settings)(metrics => check(metrics, labels, terminal()))
    }
  }
  test("unavailable server address and port remain absent after opt-in") {
    exchange(
      req = request.withHeaders(org.http4s.Headers.empty),
      settings = config.optIntoServerAddress.optIntoServerPort,
    )(metrics => check(metrics, start, terminal()))
  }
  List(true, false).foreach { classified =>
    test(
      s"http.route is absent from active requests; classified=$classified, with no raw-path fallback"
    ) {
      exchange(settings = config.withRouteClassifier(_ => Option.when(classified)("/items/{id}")))(
        metrics =>
          check(
            metrics,
            start,
            terminal() ++ Option.when(classified)(HttpAttributes.HttpRoute("/items/{id}")),
          )
      )
    }
  }
  List(
    (ErrorClassifier.never, Status.ServiceUnavailable, None),
    (ErrorClassifier.default, Status.NotFound, Some("404")),
  ).foreach { case (classifier, status, error) =>
    test(s"custom response error classifier: ${status.code} -> $error") {
      exchange(status = status, settings = config.withErrorClassifier(classifier))(metrics =>
        check(metrics, start, terminal(status) ++ error.map(ErrorAttributes.ErrorType(_)))
      )
    }
  }

  test("every specification attribute is exercised; automatic labels override configured values") {
    val settings = config.optIntoServerAddress.optIntoServerPort
      .withRouteClassifier(_ => Some("/items/{id}"))
      .withAttributes(
        Attributes(Attribute("component", "server"), Attribute("operation", "shared"))
      )
      .withRequestAttributes(_ =>
        Attributes(
          Attribute("operation", "fetch"),
          NetworkAttributes.NetworkProtocolName("http"),
          UserAgentExperimentalAttributes.UserAgentSyntheticType("test"),
          HttpAttributes.HttpRequestMethod("override"),
          UrlAttributes.UrlScheme("override"),
          ServerAttributes.ServerAddress("override"),
          ServerAttributes.ServerPort(1L),
        )
      )
    val labels = start ++ Attributes(
      ServerAttributes.ServerAddress("example.test"),
      ServerAttributes.ServerPort(443L),
      NetworkAttributes.NetworkProtocolName("http"),
      UserAgentExperimentalAttributes.UserAgentSyntheticType("test"),
      Attribute("component", "server"),
      Attribute("operation", "fetch"),
    )
    exchange(status = Status.ServiceUnavailable, settings = settings) { metrics =>
      check(
        metrics,
        labels,
        terminal(Status.ServiceUnavailable) ++ Attributes(
          ErrorAttributes.ErrorType("503"),
          HttpAttributes.HttpRoute("/items/{id}"),
        ),
      )
      List(duration, active, requestSize, responseSize).foreach { spec =>
        val keys = spec.attributeSpecs.map(_.key).toSet
        assertMetrics(
          metrics,
          MetricExpectation
            .name(spec.name)
            .where("all specification attributes exercised")(metric =>
              metric.data.points.forall(point => keys.subsetOf(point.attributes.map(_.key).toSet))
            ),
        )
      }
    }
  }

  private def terminal(status: Status = Status.Ok, version: String = "1.1"): Attributes =
    Attributes(
      HttpAttributes.HttpResponseStatusCode(status.code.toLong),
      NetworkAttributes.NetworkProtocolVersion(version),
    )

  private def metadata(spec: MetricSpec, expected: MetricExpectation): MetricExpectation =
    expected
      .unit(spec.unit)
      .description(spec.description)
      .scope(
        InstrumentationScopeExpectation
          .name("org.http4s.otel4s.middleware.server")
          .schemaUrl(SchemaUrls.V1_43_0)
      )

  private def check(metrics: List[MetricData], labels: Attributes, completion: Attributes)(implicit
      loc: Location
  ): Unit =
    assertMetrics(
      metrics,
      metadata(active, activeMetric(0L, labels)),
      metadata(duration, durationMetric(labels ++ completion)),
      metadata(
        requestSize,
        MetricExpectation
          .histogram(requestSize.name)
          .exactlyPoints(PointExpectation.histogram.count(1L).attributesExact(labels ++ completion)),
      ),
      metadata(
        responseSize,
        MetricExpectation
          .histogram(responseSize.name)
          .exactlyPoints(PointExpectation.histogram.count(1L).attributesExact(labels ++ completion)),
      ),
    )

  private def exchange(
      req: Request[IO] = request,
      status: Status = Status.Ok,
      settings: ServerMetricsConfig = config,
  )(verify: List[MetricData] => Unit): IO[Unit] =
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ServerMetrics.middleware[IO](kit, settings)
        response <- middleware(
          HttpApp[IO](_ =>
            IO.pure(Response[IO](status).putHeaders(Header.Raw(ci"Content-Length", "5")))
          )
        )(req.putHeaders(Header.Raw(ci"Content-Length", "3")))
        _ <- response.body.compile.drain
        metrics <- kit.collectMetrics
      } yield verify(metrics)
    }

}
