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
import org.http4s.server.middleware.Metrics
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
        for {
          metricsOps <- {
            implicit val MP: MeterProvider[IO] = testkit.meterProvider
            OtelMetrics.serverMetricsOps[IO]()
          }
          _ <- {
            val fakeServer =
              HttpRoutes[IO](e => OptionT.liftF(e.body.compile.drain.as(Response[IO](Status.Ok))))
            val meteredServer = Metrics[IO](metricsOps)(fakeServer)

            meteredServer
              .run(Request[IO](Method.GET))
              .semiflatMap(_.body.compile.drain)
              .value
          }
          metrics <- testkit.collectMetrics
        } yield assertMetrics(
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
        )
      }
  }

  private def assertMetrics(metrics: List[MetricData], expectations: MetricExpectation*): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(
        mismatches => fail(MetricExpectations.format(mismatches)),
        identity,
      )
}
