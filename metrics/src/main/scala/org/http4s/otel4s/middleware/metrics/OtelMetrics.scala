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
package otel4s.middleware
package metrics

import cats.Monad
import cats.syntax.all._
import org.http4s.metrics.MetricsOps
import org.http4s.metrics.TerminationType
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics._
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes

/** [[http4s.metrics.MetricsOps]] algebra capable of recording OpenTelemetry metrics
  */
object OtelMetrics {

  /** Creates a [[http4s.metrics.MetricsOps]] for clients that supports OpenTelemetry metrics.
    *
    * Registers the following metrics:
    *
    * http.client.request.duration - Histogram
    *
    * http.client.active_requests - UpDownCounter
    *
    * http.client.abnormal_terminations - Histogram
    *
    * https://opentelemetry.io/docs/specs/semconv/http/http-metrics/
    *
    * @param attributes additional [[org.typelevel.otel4s.Attributes]] that are added to all metrics
    * @param responseDurationSecondsHistogramBuckets histogram buckets for the response duration metrics
    */
  def clientMetricsOps[F[_]: Monad: Meter](
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries = DefaultHistogramBuckets,
  ): F[MetricsOps[F]] =
    metricsOps(
      "client",
      attributes,
      responseDurationSecondsHistogramBuckets,
    )

  /** Creates a [[http4s.metrics.MetricsOps]] for servers that supports OpenTelemetry metrics.
    *
    * Registers the following metrics:
    *
    * http.server.request.duration - Histogram
    *
    * http.server.active_requests - UpDownCounter
    *
    * http.server.abnormal_terminations - Histogram
    *
    * https://opentelemetry.io/docs/specs/semconv/http/http-metrics/
    *
    * @param attributes additional [[org.typelevel.otel4s.Attributes]] that are added to all metrics
    * @param responseDurationSecondsHistogramBuckets histogram buckets for the response duration metrics
    */
  def serverMetricsOps[F[_]: Monad: Meter](
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries = DefaultHistogramBuckets,
  ): F[MetricsOps[F]] =
    metricsOps(
      "server",
      attributes,
      responseDurationSecondsHistogramBuckets,
    )

  private def metricsOps[F[_]: Monad: Meter](
      kind: String,
      attributes: Attributes,
      responseDurationSecondsHistogramBuckets: BucketBoundaries,
  ): F[MetricsOps[F]] =
    for {
      metrics <- createMetricsCollection(
        kind,
        responseDurationSecondsHistogramBuckets,
      )
    } yield createMetricsOps(
      metrics,
      attributes,
    )

  private def createMetricsOps[F[_]](
      metrics: MetricsCollection[F],
      attributes: Attributes,
  ): MetricsOps[F] =
    new MetricsOps[F] {
      override def increaseActiveRequests(classifier: Option[String]): F[Unit] =
        metrics.activeRequests
          .inc(
            attributes
              .added(TypedMetricAttributes.classifier(classifier))
          )

      override def decreaseActiveRequests(classifier: Option[String]): F[Unit] =
        metrics.activeRequests
          .dec(
            attributes
              .added(TypedMetricAttributes.classifier(classifier))
          )

      override def recordHeadersTime(
          method: Method,
          elapsed: Long,
          classifier: Option[String],
      ): F[Unit] =
        metrics.requestDuration
          .record(
            secondsFromNanos(elapsed),
            attributes
              .added(TypedMetricAttributes.classifier(classifier))
              .added(TypedAttributes.httpRequestMethod(method))
              .added(TypedMetricAttributes.httpPhase(Phase.Headers)),
          )

      override def recordTotalTime(
          method: Method,
          status: Status,
          elapsed: Long,
          classifier: Option[String],
      ): F[Unit] =
        metrics.requestDuration
          .record(
            secondsFromNanos(elapsed),
            attributes
              .added(TypedMetricAttributes.classifier(classifier))
              .added(TypedAttributes.httpRequestMethod(method))
              .added(TypedAttributes.httpResponseStatusCode(status))
              .added(TypedMetricAttributes.httpPhase(Phase.Body)),
          )

      override def recordAbnormalTermination(
          elapsed: Long,
          terminationType: TerminationType,
          classifier: Option[String],
      ): F[Unit] =
        metrics.abnormalTerminations
          .record(
            secondsFromNanos(elapsed),
            attributes
              .added(TypedMetricAttributes.classifier(classifier))
              .added(TypedMetricAttributes.errorType(terminationType)),
          )

      private def secondsFromNanos(nanos: Long): Double =
        nanos / 1000000000.0
    }

  private def createMetricsCollection[F[_]: Monad: Meter](
      kind: String,
      responseDurationSecondsHistogramBuckets: BucketBoundaries,
  ): F[MetricsCollection[F]] = {
    val requestDuration: F[Histogram[F, Double]] =
      Meter[F]
        .histogram[Double](s"http.$kind.request.duration")
        .withUnit("s")
        .withDescription(s"Duration of HTTP $kind requests.")
        .withExplicitBucketBoundaries(responseDurationSecondsHistogramBuckets)
        .create

    val activeRequests: F[UpDownCounter[F, Long]] =
      Meter[F]
        .upDownCounter[Long](s"http.$kind.active_requests")
        .withUnit("{request}")
        .withDescription("Number of active HTTP requests.")
        .create

    val abnormalTerminations: F[Histogram[F, Double]] =
      Meter[F]
        .histogram[Double](s"http.$kind.abnormal_terminations")
        .withUnit("s")
        .withDescription(s"Duration of HTTP $kind abnormal terminations.")
        .withExplicitBucketBoundaries(responseDurationSecondsHistogramBuckets)
        .create

    (requestDuration, activeRequests, abnormalTerminations).mapN(MetricsCollection.apply)
  }

  private val DefaultHistogramBuckets: BucketBoundaries =
    BucketBoundaries(Vector(.005, .01, .025, .05, .075, .1, .25, .5, .75, 1, 2.5, 5, 7.5, 10))

  final case class MetricsCollection[F[_]](
      requestDuration: Histogram[F, Double],
      activeRequests: UpDownCounter[F, Long],
      abnormalTerminations: Histogram[F, Double],
  )

  private sealed trait Phase

  private object Phase {
    case object Headers extends Phase

    case object Body extends Phase
  }

  private object TypedMetricAttributes {
    private val Classifier: AttributeKey[String] = AttributeKey.string("classifier")

    def classifier(string: Option[String]): Attribute[String] =
      Classifier(string.getOrElse(""))

    private val HttpPhase: AttributeKey[String] = AttributeKey.string("http.phase")

    def httpPhase(s: Phase): Attribute[String] =
      HttpPhase(s match {
        case Phase.Headers => "headers"
        case Phase.Body => "body"
      })

    def errorType(terminationType: TerminationType): Attribute[String] =
      ErrorAttributes.ErrorType(terminationType match {
        case TerminationType.Abnormal(e) => e.getClass.getName
        case TerminationType.Error(e) => e.getClass.getName
        case TerminationType.Canceled => "cancel"
        case TerminationType.Timeout => "timeout"
      })
  }
}
