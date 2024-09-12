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
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.Meter
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.MetricPoints
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.testkit.metrics.MetricsTestkit
import org.typelevel.otel4s.semconv.MetricSpec
import org.typelevel.otel4s.semconv.Requirement
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

class OtelMetricsTests extends CatsEffectSuite {
  test("OtelMetrics") {
    MetricsTestkit
      .inMemory[IO]()
      .use { testkit =>
        for {
          meterIO <- testkit.meterProvider.get("meter")
          metricsOps <- {
            implicit val meter: Meter[IO] = meterIO
            OtelMetrics.serverMetricsOps[IO]()
          }
          _ <- {
            val fakeServer =
              HttpRoutes[IO](e => OptionT.liftF(e.body.compile.drain.as(Response[IO](Status.Ok))))
            val meteredServer = ServerMetrics[IO](metricsOps)(fakeServer)

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

  test("server semantic test") {
    val specs = List(
      HttpMetrics.ServerRequestDuration,
      HttpExperimentalMetrics.ServerActiveRequests,
    )

    MetricsTestkit.inMemory[IO]().use { testkit =>
      testkit.meterProvider.get("meter").flatMap { implicit meter =>
        val fakeServer =
          HttpRoutes[IO](e => OptionT.liftF(e.body.compile.drain.as(Response[IO](Status.Ok))))

        for {
          metricsOps <- OtelMetrics.serverMetricsOps[IO]()
          server = ServerMetrics[IO](metricsOps)(fakeServer)

          _ <- server.run(Request[IO](Method.GET)).semiflatMap(_.body.compile.drain).value
          metrics <- testkit.collectMetrics
        } yield specs.foreach(spec => specTest(metrics, spec))
      }
    }
  }

  test("client semantic test") {
    val specs = List(
      HttpMetrics.ClientRequestDuration,
      HttpExperimentalMetrics.ClientActiveRequests,
    )

    MetricsTestkit.inMemory[IO]().use { testkit =>
      testkit.meterProvider.get("meter").flatMap { implicit meter =>
        val fakeServer =
          HttpRoutes[IO](e => OptionT.liftF(e.body.compile.drain.as(Response[IO](Status.Ok))))

        for {
          clientOps <- OtelMetrics.clientMetricsOps[IO]()
          client = ClientMetrics[IO](clientOps)(Client.fromHttpApp(fakeServer.orNotFound))

          _ <- client.run(Request[IO](Method.GET)).use(_.body.compile.drain)
          metrics <- testkit.collectMetrics
        } yield specs.foreach(spec => specTest(metrics, spec))
      }
    }
  }

  private def specTest(metrics: List[MetricData], spec: MetricSpec): Unit = {
    val metric = metrics.find(_.name == spec.name)
    assert(
      metric.isDefined,
      s"${spec.name} metric is missing. Available [${metrics.map(_.name).mkString(", ")}]",
    )

    val clue = s"[${spec.name}] has a mismatched property"

    metric.foreach { md =>
      assertEquals(md.name, spec.name, clue)
      assertEquals(md.description, Some(spec.description), clue)
      assertEquals(md.unit, Some(spec.unit), clue)

      val required = spec.attributeSpecs
        .filter(_.requirement.level == Requirement.Level.Required)
        .map(_.key)
        .toSet

      val current = md.data.points.toVector
        .flatMap(_.attributes.map(_.key))
        .filter(key => required.contains(key))
        .toSet

      assertEquals(current, required, clue)
    }
  }

}
