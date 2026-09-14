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

import cats.Applicative
import cats.Monad
import cats.syntax.all._
import org.http4s.metrics.MetricsOps2
import org.http4s.metrics.TerminationType
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics._
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

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
  def clientMetricsOps[F[_]: Monad: MeterProvider](
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries = DefaultHistogramBuckets,
  ): F[MetricsOps2[F]] =
    metricsOps(
      "client",
      _ => Monad[F].pure(attributes),
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
  def serverMetricsOps[F[_]: Monad: MeterProvider](
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries = DefaultHistogramBuckets,
  ): F[MetricsOps2[F]] =
    metricsOps(
      "server",
      _ => Monad[F].pure(attributes),
      responseDurationSecondsHistogramBuckets,
    )

  private def metricsOps[F[_]: Monad: MeterProvider](
      kind: String,
      attributes: RequestPrelude => F[Attributes],
      responseDurationSecondsHistogramBuckets: BucketBoundaries,
  ): F[MetricsOps2[F]] =
    for {
      meter <- MeterProvider[F]
        .meter(s"org.http4s.otel4s.middleware.$kind")
        .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
        .get
      metrics <- {
        implicit val M: Meter[F] = meter
        createMetricsCollection(
          kind,
          responseDurationSecondsHistogramBuckets,
        )
      }
    } yield createMetricsOps(
      metrics,
      attributes,
    )

  private def createMetricsOps[F[_]: Applicative](
      metrics: MetricsCollection[F],
      contextAttributes: RequestPrelude => F[Attributes],
  ): MetricsOps2[F] =
    new MetricsOps2[F] {
      type Context = Attributes

      override def createContext(request: RequestPrelude): F[Attributes] =
        contextAttributes(request).map { attributes =>
          attributes
            .added(TypedAttributes.httpRequestMethod(request.method))
            .concat(TypedAttributes.urlScheme(request.uri.scheme))
            .concat(TypedAttributes.serverAddress(request.uri.host))
        }

      override def increaseActiveRequests(request: RequestPrelude, context: Attributes): F[Unit] =
        metrics.activeRequests.inc(context)

      override def decreaseActiveRequests(request: RequestPrelude, context: Attributes): F[Unit] =
        metrics.activeRequests.dec(context)

      override def recordHeadersTime(
          request: RequestPrelude,
          elapsed: FiniteDuration,
          context: Attributes,
      ): F[Unit] =
        metrics.requestDuration.record(
          elapsed.toUnit(TimeUnit.NANOSECONDS),
          context
            .added(TypedMetricAttributes.httpPhase(Phase.Headers)),
        )

      override def recordTotalTime(
          request: RequestPrelude,
          response: Option[ResponsePrelude],
          terminationType: Option[TerminationType],
          elapsed: FiniteDuration,
          context: Attributes,
      ): F[Unit] =
        metrics.requestDuration.record(
          elapsed.toUnit(TimeUnit.NANOSECONDS),
          context
            .concat(TypedAttributes.httpResponseStatusCode(response.map(_.status)))
            .concat(TypedMetricAttributes.errorType(terminationType))
            .added(TypedMetricAttributes.httpPhase(Phase.Body)),
        )

      override def recordRequestBodySize(
          request: RequestPrelude,
          response: Option[ResponsePrelude],
          terminationType: Option[TerminationType],
          bodySizeBytes: Long,
          context: Attributes,
      ): F[Unit] =
        metrics.requestBodySize
          .record(
            bodySizeBytes,
            context
              .concat(TypedAttributes.httpResponseStatusCode(response.map(_.status)))
              .concat(TypedMetricAttributes.errorType(terminationType)),
          )

      override def recordResponseBodySize(
          request: RequestPrelude,
          response: ResponsePrelude,
          terminationType: Option[TerminationType],
          bodySizeBytes: Long,
          context: Attributes,
      ): F[Unit] =
        metrics.responseBodySize
          .record(
            bodySizeBytes,
            context
              .added(TypedAttributes.httpResponseStatusCode(response.status))
              .concat(TypedMetricAttributes.errorType(terminationType)),
          )

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
        .withDescription(
          s"Number of active HTTP ${if (kind == "client") "" else "server "}requests."
        )
        .create

    val requestBodySize: F[Histogram[F, Long]] =
      Meter[F]
        .histogram[Long](s"http.$kind.request.body.size")
        .withUnit("By")
        .withDescription(s"Size of HTTP $kind request bodies.")
        .withExplicitBucketBoundaries(responseDurationSecondsHistogramBuckets)
        .create

    val responseBodySize: F[Histogram[F, Long]] =
      Meter[F]
        .histogram[Long](s"http.$kind.response.body.size")
        .withUnit("By")
        .withDescription(s"Size of HTTP $kind response bodies.")
        .withExplicitBucketBoundaries(responseDurationSecondsHistogramBuckets)
        .create

    (requestDuration, activeRequests, requestBodySize, responseBodySize).mapN(
      MetricsCollection.apply
    )
  }

  private val DefaultHistogramBuckets: BucketBoundaries =
    BucketBoundaries(.005, .01, .025, .05, .075, .1, .25, .5, .75, 1, 2.5, 5, 7.5, 10)

  final case class MetricsCollection[F[_]](
      requestDuration: Histogram[F, Double],
      activeRequests: UpDownCounter[F, Long],
      requestBodySize: Histogram[F, Long],
      responseBodySize: Histogram[F, Long],
  )

  private sealed trait Phase

  private object Phase {
    case object Headers extends Phase

    case object Body extends Phase
  }

  private object TypedMetricAttributes {

    private val HttpPhase: AttributeKey[String] = AttributeKey.string("http.phase")

    def httpPhase(s: Phase): Attribute[String] =
      HttpPhase(s match {
        case Phase.Headers => "headers"
        case Phase.Body => "body"
      })

    def errorType(terminationType: Option[TerminationType]): Option[Attribute[String]] =
      ErrorAttributes.ErrorType.maybe(terminationType.map {
        case TerminationType.Abnormal(e) => e.getClass.getName
        case TerminationType.Error(e) => e.getClass.getName
        case TerminationType.Canceled => "cancel"
        case TerminationType.Timeout => "timeout"
      })
  }
}
