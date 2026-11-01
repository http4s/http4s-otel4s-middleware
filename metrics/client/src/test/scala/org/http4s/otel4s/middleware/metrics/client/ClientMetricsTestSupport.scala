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

import munit.CatsEffectSuite
import munit.Location
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

private[client] trait ClientMetricsTestSupport { self: CatsEffectSuite =>

  protected def activeMetric(value: Long, attributes: Attributes): MetricExpectation =
    MetricExpectation
      .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
      .exactlyPoints(PointExpectation.numeric(value).attributesExact(attributes))

  protected def durationMetric(attributes: Attributes): MetricExpectation =
    MetricExpectation
      .histogram(HttpMetrics.ClientRequestDuration.name)
      .exactlyPoints(PointExpectation.histogram.count(1L).attributesExact(attributes))

  protected def assertNoMetrics(metrics: List[MetricData])(implicit loc: Location): Unit =
    List(
      HttpMetrics.ClientRequestDuration.name,
      HttpExperimentalMetrics.ClientActiveRequests.name,
      HttpExperimentalMetrics.ClientRequestBodySize.name,
      HttpExperimentalMetrics.ClientResponseBodySize.name,
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
