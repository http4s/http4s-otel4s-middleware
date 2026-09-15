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
package otel4s.middleware.metrics

import cats.data.OptionT
import cats.effect.IO
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress
import munit.CatsEffectSuite
import munit.Location
import org.http4s.client.Client
import org.http4s.client.middleware.{Metrics => ClientMetrics}
import org.http4s.headers.Host
import org.http4s.server.middleware.{Metrics => ServerMetrics}
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.AttributesExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.MetricsTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.PointSetExpectation
import org.typelevel.otel4s.semconv.MetricSpec
import org.typelevel.otel4s.semconv.Requirement
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class OtelMetricsTests extends CatsEffectSuite {

  private val ServerAttribute = Attribute("org.http4s.otel4s.middleware.metrics", "server")
  private val ClientAttribute = Attribute("org.http4s.otel4s.middleware.metrics", "client")

  test("OtelMetrics") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        implicit val meterProvider: MeterProvider[IO] = testkit.meterProvider

        for {
          serverMetricsOps <- OtelMetrics.serverMetricsOps[IO](
            ServerMetricsConfig.all.withAdditionalAttributes(Attributes(ServerAttribute))
          )
          clientMetricsOps <- OtelMetrics.clientMetricsOps[IO](
            ClientMetricsConfig.all.withAdditionalAttributes(Attributes(ClientAttribute))
          )

          activeServerMetrics <- IO.deferred[List[MetricData]]
          activeClientMetrics <- IO.deferred[List[MetricData]]

          _ <- {
            val fakeServer =
              HttpRoutes[IO](e =>
                OptionT.liftF(
                  testkit.collectMetrics.flatMap(activeServerMetrics.complete) >>
                    e.body.compile.drain.as(
                      Response[IO](Status.Ok).withBodyStream(fs2.Stream(1, 2, 3))
                    )
                )
              )

            val meteredServer = ServerMetrics[IO](serverMetricsOps)(fakeServer)

            val meteredClient =
              ClientMetrics[IO](clientMetricsOps)(Client.fromHttpApp(meteredServer.orNotFound))

            meteredClient
              .run(Request[IO](Method.GET, uri = uri"https://http4s.org"))
              .use { r =>
                testkit.collectMetrics.flatMap(activeClientMetrics.complete) >>
                  r.body.compile.drain
              }
          }

          activeServer <- activeServerMetrics.get
          activeClient <- activeClientMetrics.get
          metrics <- testkit.collectMetrics
        } yield {
          assertMetrics(activeServer, activeRequestsExpectation("server", 1L))
          assertMetrics(activeClient, activeRequestsExpectation("client", 1L))
          assertMetrics(
            metrics,
            // server
            MetricExpectation
              .sum[Long]("http.server.active_requests")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation
                    .numeric(0L)
                    .attributesExact(
                      ServerAttribute,
                      ServerAttributes.ServerAddress("http4s.org"),
                      ServerAttributes.ServerPort(443L),
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      UrlAttributes.UrlScheme("https"),
                    )
                )
              ),
            MetricExpectation
              .histogram("http.server.response.headers.duration")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      ServerAttribute,
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      NetworkAttributes.NetworkProtocolVersion("1.1"),
                      ServerAttributes.ServerAddress("http4s.org"),
                      ServerAttributes.ServerPort(443L),
                      UrlAttributes.UrlScheme("https"),
                    )
                )
              ),
            MetricExpectation
              .histogram("http.server.request.duration")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      ServerAttribute,
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      HttpAttributes.HttpResponseStatusCode(200L),
                      NetworkAttributes.NetworkProtocolVersion("1.1"),
                      ServerAttributes.ServerAddress("http4s.org"),
                      ServerAttributes.ServerPort(443L),
                      UrlAttributes.UrlScheme("https"),
                    )
                )
              ),
            // client
            MetricExpectation
              .sum[Long]("http.client.active_requests")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation
                    .numeric(0L)
                    .attributesExact(
                      ClientAttribute,
                      ServerAttributes.ServerAddress("http4s.org"),
                      ServerAttributes.ServerPort(443L),
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      UrlAttributes.UrlScheme("https"),
                    )
                )
              ),
            MetricExpectation
              .histogram("http.client.response.headers.duration")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      ClientAttribute,
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      NetworkAttributes.NetworkProtocolVersion("1.1"),
                      ServerAttributes.ServerAddress("http4s.org"),
                      ServerAttributes.ServerPort(443L),
                      UrlAttributes.UrlScheme("https"),
                    )
                )
              ),
            MetricExpectation
              .histogram("http.client.request.duration")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      ClientAttribute,
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      HttpAttributes.HttpResponseStatusCode(200L),
                      NetworkAttributes.NetworkProtocolVersion("1.1"),
                      ServerAttributes.ServerAddress("http4s.org"),
                      ServerAttributes.ServerPort(443L),
                      UrlAttributes.UrlScheme("https"),
                    )
                )
              ),
          )
        }
      }
  }

  test("OtelMetrics: semantic conventions") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        implicit val meterProvider: MeterProvider[IO] = testkit.meterProvider

        for {
          serverMetricsOps <- OtelMetrics.serverMetricsOps[IO](
            ServerMetricsConfig.all
          )
          clientMetricsOps <- OtelMetrics.clientMetricsOps[IO](
            ClientMetricsConfig.all
          )

          activeServerMetrics <- IO.deferred[List[MetricData]]
          activeClientMetrics <- IO.deferred[List[MetricData]]

          _ <- {
            val fakeServer =
              HttpRoutes[IO](e =>
                OptionT.liftF(
                  testkit.collectMetrics.flatMap(activeServerMetrics.complete) >>
                    e.body.compile.drain
                      .as(Response[IO](Status.Ok).withBodyStream(fs2.Stream(1, 2, 3)))
                )
              )

            val meteredServer = ServerMetrics[IO](serverMetricsOps)(fakeServer)

            val meteredClient =
              ClientMetrics[IO](clientMetricsOps)(Client.fromHttpApp(meteredServer.orNotFound))

            meteredClient
              .run(Request[IO](Method.GET, uri = uri"https://http4s.org"))
              .use { r =>
                testkit.collectMetrics.flatMap(activeClientMetrics.complete) >>
                  r.body.compile.drain
              }
          }

          activeServer <- activeServerMetrics.get
          activeClient <- activeClientMetrics.get
          metrics <- testkit.collectMetrics
        } yield {
          assertMetrics(
            activeServer,
            numericExpectation(HttpExperimentalMetrics.ServerActiveRequests, 1L),
          )

          assertMetrics(
            activeClient,
            numericExpectation(HttpExperimentalMetrics.ClientActiveRequests, 1L),
          )

          assertMetrics(
            metrics,
            // server
            histogramExpectation(HttpMetrics.ServerRequestDuration),
            histogramExpectation(HttpExperimentalMetrics.ServerRequestBodySize),
            histogramExpectation(HttpExperimentalMetrics.ServerResponseBodySize),
            // client
            histogramExpectation(HttpMetrics.ClientRequestDuration),
            histogramExpectation(HttpExperimentalMetrics.ClientRequestBodySize),
            histogramExpectation(HttpExperimentalMetrics.ClientResponseBodySize),
          )
        }
      }
  }

  test("config defaults do not emit opt-in metrics") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        implicit val meterProvider: MeterProvider[IO] = testkit.meterProvider

        for {
          serverMetricsOps <- OtelMetrics.serverMetricsOps[IO](ServerMetricsConfig.recommended)
          clientMetricsOps <- OtelMetrics.clientMetricsOps[IO](ClientMetricsConfig.recommended)
          server = ServerMetrics[IO](serverMetricsOps)(HttpRoutes.of[IO] { case _ =>
            IO.pure(Response[IO](Status.Ok))
          }).orNotFound
          client = ClientMetrics[IO](clientMetricsOps)(Client.fromHttpApp(server))
          _ <- client
            .run(Request[IO](Method.GET, uri = uri"https://http4s.org"))
            .use(_ => IO.unit)
          metrics <- testkit.collectMetrics
        } yield assertEquals(
          metrics.map(_.name).sorted,
          List(
            "http.client.request.duration",
            "http.server.request.duration",
          ),
        )
      }
  }

  test("server connection security supplies the scheme and default port") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        implicit val meterProvider: MeterProvider[IO] = testkit.meterProvider

        val connection = Request.Connection(
          local = SocketAddress(Ipv4Address.fromBytes(127, 0, 0, 1), Port.fromInt(4321).get),
          remote = SocketAddress(Ipv4Address.fromBytes(192, 168, 1, 1), Port.fromInt(1234).get),
          secure = true,
        )

        for {
          metricsOps <- OtelMetrics.serverMetricsOps[IO](ServerMetricsConfig.all)
          server = ServerMetrics[IO](metricsOps)(HttpRoutes.of[IO] { case _ =>
            IO.pure(Response[IO](Status.Ok))
          }).orNotFound
          _ <- server
            .run(
              Request[IO]()
                .putHeaders(Host("example.com"))
                .withAttribute(
                  Request.Keys.ConnectionInfo,
                  connection,
                )
            )
            .flatMap(_.body.compile.drain)
          metrics <- testkit.collectMetrics
        } yield assertMetrics(
          metrics,
          MetricExpectation
            .histogram("http.server.request.duration")
            .points(
              PointSetExpectation.exactly(
                PointExpectation.histogram
                  .count(1L)
                  .attributesExact(
                    HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                    HttpAttributes.HttpResponseStatusCode(200L),
                    NetworkAttributes.NetworkProtocolVersion("1.1"),
                    ServerAttributes.ServerAddress("example.com"),
                    ServerAttributes.ServerPort(443L),
                    UrlAttributes.UrlScheme("https"),
                  )
              )
            ),
        )
      }
  }

  test("config presets") {
    assert(!ClientMetricsConfig.minimal.networkProtocolVersionEnabled)
    assert(ClientMetricsConfig.recommended.networkProtocolVersionEnabled)
    assert(!ClientMetricsConfig.recommended.activeRequestsEnabled)
    assert(ClientMetricsConfig.all.responseHeadersDurationEnabled)
    assert(ClientMetricsConfig.all.activeRequestsEnabled)
    assert(ClientMetricsConfig.all.requestBodySizeEnabled)
    assert(ClientMetricsConfig.all.responseBodySizeEnabled)
    assert(ClientMetricsConfig.all.urlSchemeEnabled)

    assert(!ServerMetricsConfig.minimal.networkProtocolVersionEnabled)
    assert(ServerMetricsConfig.recommended.networkProtocolVersionEnabled)
    assert(!ServerMetricsConfig.recommended.activeRequestsEnabled)
    assert(ServerMetricsConfig.all.responseHeadersDurationEnabled)
    assert(ServerMetricsConfig.all.activeRequestsEnabled)
    assert(ServerMetricsConfig.all.requestBodySizeEnabled)
    assert(ServerMetricsConfig.all.responseBodySizeEnabled)
    assert(ServerMetricsConfig.all.serverAddressAndPortEnabled)
  }

  private val IgnoredRequiredAttributes: Set[AttributeKey[_]] = Set.empty

  private def histogramExpectation(spec: MetricSpec): MetricExpectation.Histogram = {
    val requiredKeys = spec.attributeSpecs.collect {
      case attribute
          if attribute.requirement.level == Requirement.Level.Required &&
            !IgnoredRequiredAttributes.contains(attribute.key) =>
        attribute.key
    }

    val requiredAttributes =
      AttributesExpectation.where(
        s"required attributes: ${requiredKeys.map(_.name).sorted.mkString(", ")}"
      ) { attributes =>
        requiredKeys.forall { key =>
          attributes.exists(_.key == key)
        }
      }

    MetricExpectation
      .histogram(spec.name)
      .description(spec.description)
      .unit(spec.unit)
      .points(
        PointSetExpectation.forall(
          PointExpectation.histogram.attributes(requiredAttributes)
        )
      )
  }

  private def numericExpectation(spec: MetricSpec, value: Long): MetricExpectation.Numeric[Long] = {
    val requiredKeys = spec.attributeSpecs.collect {
      case attribute
          if attribute.requirement.level == Requirement.Level.Required &&
            !IgnoredRequiredAttributes.contains(attribute.key) =>
        attribute.key
    }

    val requiredAttributes =
      AttributesExpectation.where(
        s"required attributes: ${requiredKeys.map(_.name).sorted.mkString(", ")}"
      ) { attributes =>
        requiredKeys.forall { key =>
          attributes.exists(_.key == key)
        }
      }

    MetricExpectation
      .sum[Long](spec.name)
      .description(spec.description)
      .unit(spec.unit)
      .points(
        PointSetExpectation.forall(
          PointExpectation.numeric(value).attributes(requiredAttributes)
        )
      )
  }

  private def activeRequestsExpectation(kind: String, value: Long): MetricExpectation =
    MetricExpectation
      .sum[Long](s"http.$kind.active_requests")
      .points(
        PointSetExpectation.exactly(
          PointExpectation
            .numeric(value)
            .attributesExact(
              if (kind == "client") ClientAttribute else ServerAttribute,
              ServerAttributes.ServerAddress("http4s.org"),
              ServerAttributes.ServerPort(443L),
              HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
              UrlAttributes.UrlScheme("https"),
            )
        )
      )

  private def assertMetrics(
      metrics: List[MetricData],
      expectations: MetricExpectation*
  )(implicit loc: Location): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(
        mismatches => fail(MetricExpectations.format(mismatches)),
        identity,
      )
}
