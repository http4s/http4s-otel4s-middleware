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
import munit.CatsEffectSuite
import munit.Location
import org.http4s.client.Client
import org.http4s.client.middleware.{Metrics => ClientMetrics}
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
          serverMetricsOps <- OtelMetrics.serverMetricsOps[IO](Attributes(ServerAttribute))
          clientMetricsOps <- OtelMetrics.clientMetricsOps[IO](Attributes(ClientAttribute))

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
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
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
                      Attribute("http.phase", "headers"),
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      ServerAttributes.ServerAddress("http4s.org"),
                      UrlAttributes.UrlScheme("https"),
                    ),
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      ServerAttribute,
                      Attribute("http.phase", "body"),
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      HttpAttributes.HttpResponseStatusCode(200L),
                      ServerAttributes.ServerAddress("http4s.org"),
                      UrlAttributes.UrlScheme("https"),
                    ),
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
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
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
                      Attribute("http.phase", "headers"),
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      ServerAttributes.ServerAddress("http4s.org"),
                      UrlAttributes.UrlScheme("https"),
                    ),
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      ClientAttribute,
                      Attribute("http.phase", "body"),
                      HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
                      HttpAttributes.HttpResponseStatusCode(200L),
                      ServerAttributes.ServerAddress("http4s.org"),
                      UrlAttributes.UrlScheme("https"),
                    ),
                )
              ),
          )
        }
      }
  }

  test("OtelMetrics: semanic conventions") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        implicit val meterProvider: MeterProvider[IO] = testkit.meterProvider

        for {
          serverMetricsOps <- OtelMetrics.serverMetricsOps[IO]()
          clientMetricsOps <- OtelMetrics.clientMetricsOps[IO]()

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

  // we cannot reliably populate `server.port`, so skipping it
  private val IgnoredRequiredAttributes: Set[AttributeKey[_]] =
    Set(ServerAttributes.ServerPort)

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
