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
import io.opentelemetry.api.common.{Attributes => JAttributes}
import io.opentelemetry.sdk.metrics.data.{MetricData => JMetricData}
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.server.middleware.Metrics
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.oteljava.AttributeConverters._
import org.typelevel.otel4s.oteljava.testkit.metrics.MetricsTestkit

import scala.jdk.CollectionConverters._

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
          metrics <- testkit.collectMetrics[JMetricData]
        } yield {
          def attributes(attrs: JAttributes): Map[String, String] =
            attrs.toScala.toSeq.map(e => e.key.name -> e.value.toString).toMap

          val activeRequests = metrics.find(_.getName == "http.server.active_requests").get
          val activeRequestsDataPoints: Map[Map[String, String], Long] =
            activeRequests.getLongSumData.getPoints.asScala.toList
              .map(e => attributes(e.getAttributes) -> e.getValue)
              .toMap

          val requestDuration = metrics.find(_.getName == "http.server.request.duration").get
          val requestDurationDataPoints: Map[Map[String, String], Long] =
            requestDuration.getHistogramData.getPoints.asScala.toList
              .map(e => attributes(e.getAttributes) -> e.getCount)
              .toMap

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
