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

import munit.CatsEffectSuite
import munit.Location
import org.http4s.otel4s.middleware.metrics.MetricsDefaults
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

private[server] trait ServerMetricsTestSupport { self: CatsEffectSuite =>

  protected def activeMetric(value: Long, attributes: Attributes): MetricExpectation =
    MetricExpectation
      .sum[Long](HttpExperimentalMetrics.ServerActiveRequests.name)
      .monotonic(false)
      .exactlyPoints(PointExpectation.numeric(value).attributesExact(attributes))

  protected def durationMetric(attributes: Attributes): MetricExpectation =
    MetricExpectation
      .histogram(HttpMetrics.ServerRequestDuration.name)
      .exactlyPoints(
        PointExpectation.histogram
          .count(1L)
          .boundaries(MetricsDefaults.DurationBuckets)
          .attributesExact(attributes)
      )

  protected def durationMetric(seconds: Double, attributes: Attributes): MetricExpectation =
    MetricExpectation
      .histogram(HttpMetrics.ServerRequestDuration.name)
      .unit("s")
      .exactlyPoints(PointExpectation.histogram.count(1L).sum(seconds).attributesExact(attributes))

  protected def assertNoMetrics(metrics: List[MetricData])(implicit loc: Location): Unit =
    List(
      HttpMetrics.ServerRequestDuration.name,
      HttpExperimentalMetrics.ServerActiveRequests.name,
      HttpExperimentalMetrics.ServerRequestBodySize.name,
      HttpExperimentalMetrics.ServerResponseBodySize.name,
    ).foreach { name =>
      assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(name)), None)
    }

  protected def assertMetrics(
      metrics: List[MetricData],
      expectations: MetricExpectation*
  )(implicit loc: Location): Unit =
    MetricExpectations
      .checkAllDistinct(metrics, expectations: _*)
      .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
}
