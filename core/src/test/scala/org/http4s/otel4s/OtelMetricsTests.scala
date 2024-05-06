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

package org.http4s.otel4s

import cats.data.OptionT
import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.server.middleware.Metrics
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.sdk.metrics.data.MetricPoints
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.testkit.metrics.MetricsTestkit

class OtelMetricsTests extends CatsEffectSuite {
  test("OtelMetrics") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          meterIO <- testkit.meterProvider.get("meter")
          metricsOps <- {
            implicit val meter: Meter[IO] = meterIO
            OtelMetrics.metricsOps[IO]()
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
        } yield {
          def attributes(attrs: Attributes): Map[String, String] =
            attrs.map(a => a.key.name -> a.value.toString).toMap

          val activeRequestsDataPoints: Map[Map[String, String], Long] =
            metrics
              .find(_.name == "http.server.active_requests")
              .map(_.data)
              .collect { case sum: MetricPoints.Sum =>
                sum.points.toVector.collect { case long: PointData.LongNumber =>
                  attributes(long.attributes) -> long.value
                }.toMap
              }
              .getOrElse(Map.empty)

          val requestDurationDataPoints: Map[Map[String, String], Long] =
            metrics
              .find(_.name == "http.server.request.duration")
              .map(_.data)
              .collect { case histogram: MetricPoints.Histogram =>
                histogram.points.toVector
                  .map(e => attributes(e.attributes) -> e.stats.map(_.count).getOrElse(0L))
                  .toMap
              }
              .getOrElse(Map.empty)

          assertEquals(
            activeRequestsDataPoints,
            Map(
              Map("classifier" -> "") -> 0L
            ),
          )

          assertEquals(
            requestDurationDataPoints,
            Map(
              Map(
                "classifier" -> "",
                "http.phase" -> "headers",
                "http.request.method" -> "GET",
              ) -> 1L,
              Map(
                "classifier" -> "",
                "http.phase" -> "body",
                "http.request.method" -> "GET",
                "http.response.status_code" -> "200",
              ) -> 1L,
            ),
          )
        }
      }
  }
}
