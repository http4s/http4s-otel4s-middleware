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
import org.http4s.client.Client
import org.http4s.client.middleware.{Metrics => ClientMetrics}
import org.http4s.server.middleware.{Metrics => ServerMetrics}
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.MetricsTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.PointSetExpectation

class OtelMetricsTests extends CatsEffectSuite {
  test("OtelMetrics") {
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
                    e.body.compile.drain.as(Response[IO](Status.Ok))
                )
              )

            val meteredServer = ServerMetrics[IO](serverMetricsOps)(fakeServer)

            val meteredClient =
              ClientMetrics[IO](clientMetricsOps)(Client.fromHttpApp(meteredServer.orNotFound))

            meteredClient
              .run(Request[IO](Method.GET))
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
            MetricExpectation
              .sum[Long]("http.server.active_requests")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation
                    .numeric(0L)
                    .attributesExact(Attribute("classifier", ""))
                )
              ),
            MetricExpectation
              .histogram("http.server.request.duration")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      Attribute("classifier", ""),
                      Attribute("http.phase", "headers"),
                      Attribute("http.request.method", "GET"),
                    ),
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      Attribute("classifier", ""),
                      Attribute("http.phase", "body"),
                      Attribute("http.request.method", "GET"),
                      Attribute("http.response.status_code", 200L),
                    ),
                )
              ),
            MetricExpectation
              .sum[Long]("http.client.active_requests")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation
                    .numeric(0L)
                    .attributesExact(Attribute("classifier", ""))
                )
              ),
            MetricExpectation
              .histogram("http.client.request.duration")
              .points(
                PointSetExpectation.exactly(
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      Attribute("classifier", ""),
                      Attribute("http.phase", "headers"),
                      Attribute("http.request.method", "GET"),
                    ),
                  PointExpectation.histogram
                    .count(1L)
                    .attributesExact(
                      Attribute("classifier", ""),
                      Attribute("http.phase", "body"),
                      Attribute("http.request.method", "GET"),
                      Attribute("http.response.status_code", 200L),
                    ),
                )
              ),
          )
        }
      }
  }

  private def activeRequestsExpectation(kind: String, value: Long): MetricExpectation =
    MetricExpectation
      .sum[Long](s"http.$kind.active_requests")
      .points(
        PointSetExpectation.exactly(
          PointExpectation
            .numeric(value)
            .attributesExact(Attribute("classifier", ""))
        )
      )

  private def assertMetrics(metrics: List[MetricData], expectations: MetricExpectation*): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(
        mismatches => fail(MetricExpectations.format(mismatches)),
        identity,
      )
}
