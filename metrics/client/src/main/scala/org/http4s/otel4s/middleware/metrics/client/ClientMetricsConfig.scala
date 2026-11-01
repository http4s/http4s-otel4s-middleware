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

import org.http4s.RequestPrelude
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.client.UriTemplateClassifier
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.MetricsDefaults
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries

/** Immutable client metrics settings, independent of telemetry providers and effect types. */
sealed trait ClientMetricsConfig {
  def attributes: Attributes
  def responseDurationSecondsHistogramBuckets: BucketBoundaries
  def requestFilter: PerRequestFilter
  def requestAttributes: RequestPrelude => Attributes
  def requestBodySize: Option[BodySizeConfig]
  def responseBodySize: Option[BodySizeConfig]
  def urlSchemeEnabled: Boolean
  def activeRequestsEnabled: Boolean
  def urlTemplateClassifier: UriTemplateClassifier
  def errorClassifier: ErrorClassifier

  /** Sets attributes shared by all requests. */
  def withAttributes(attributes: Attributes): ClientMetricsConfig

  /** Sets request labels for all client metrics. Use low-cardinality values.
    * Request labels override shared labels; generated semantic attributes take precedence over both.
    */
  def withRequestAttributes(attributes: RequestPrelude => Attributes): ClientMetricsConfig

  /** Enables request-size recording from Content-Length with default byte buckets. */
  def optIntoRequestBodySize: ClientMetricsConfig

  /** Enables request-size recording with the selected source and byte buckets. */
  def optIntoRequestBodySize(config: BodySizeConfig): ClientMetricsConfig

  /** Uses Content-Length or protocol-known zero for response size, with default byte buckets. */
  def optIntoResponseBodySize: ClientMetricsConfig

  /** Enables response-size recording with the selected source and byte buckets. */
  def optIntoResponseBodySize(config: BodySizeConfig): ClientMetricsConfig

  /** Enables the experimental `http.client.active_requests` metric (disabled by default). */
  def optIntoActiveRequests: ClientMetricsConfig

  /** Adds `url.scheme` from the request URI when present. */
  def optIntoUrlScheme: ClientMetricsConfig

  /** Adds `url.template` when classified. No raw-path fallback. */
  def withUrlTemplateClassifier(classifier: UriTemplateClassifier): ClientMetricsConfig

  /** Classifies error responses; observed exceptions still take precedence. */
  def withErrorClassifier(classifier: ErrorClassifier): ClientMetricsConfig

  /** Enables metrics for matching requests; checked once per acquisition. */
  def withRequestFilter(filter: PerRequestFilter): ClientMetricsConfig

  /** Sets duration histogram buckets. */
  def withResponseDurationSecondsHistogramBuckets(buckets: BucketBoundaries): ClientMetricsConfig
}

object ClientMetricsConfig {

  /** Records duration for all requests using the standard HTTP histogram buckets, with no custom
    * labels, URL scheme or template. Active-request and body-size metrics are disabled.
    * 4xx and 5xx responses are classified as errors.
    */
  val default: ClientMetricsConfig = Impl()

  private final case class Impl(
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries = MetricsDefaults.DurationBuckets,
      requestFilter: PerRequestFilter = PerRequestFilter.alwaysEnabled,
      requestAttributes: RequestPrelude => Attributes = _ => Attributes.empty,
      requestBodySize: Option[BodySizeConfig] = None,
      responseBodySize: Option[BodySizeConfig] = None,
      urlSchemeEnabled: Boolean = false,
      activeRequestsEnabled: Boolean = false,
      urlTemplateClassifier: UriTemplateClassifier = UriTemplateClassifier.indeterminate,
      errorClassifier: ErrorClassifier = ErrorClassifier.default,
  ) extends ClientMetricsConfig {
    def withAttributes(attributes: Attributes): ClientMetricsConfig =
      copy(attributes = attributes)

    def withRequestAttributes(attributes: RequestPrelude => Attributes): ClientMetricsConfig =
      copy(requestAttributes = attributes)

    def optIntoRequestBodySize: ClientMetricsConfig =
      optIntoRequestBodySize(BodySizeConfig.contentLength)

    def optIntoRequestBodySize(config: BodySizeConfig): ClientMetricsConfig =
      copy(requestBodySize = Some(config))

    def optIntoResponseBodySize: ClientMetricsConfig =
      optIntoResponseBodySize(BodySizeConfig.contentLength)

    def optIntoResponseBodySize(config: BodySizeConfig): ClientMetricsConfig =
      copy(responseBodySize = Some(config))

    def optIntoActiveRequests: ClientMetricsConfig =
      copy(activeRequestsEnabled = true)

    def optIntoUrlScheme: ClientMetricsConfig =
      copy(urlSchemeEnabled = true)

    def withUrlTemplateClassifier(classifier: UriTemplateClassifier): ClientMetricsConfig =
      copy(urlTemplateClassifier = classifier)

    def withErrorClassifier(
        classifier: ErrorClassifier
    ): ClientMetricsConfig =
      copy(errorClassifier = classifier)

    def withRequestFilter(filter: PerRequestFilter): ClientMetricsConfig =
      copy(requestFilter = filter)

    def withResponseDurationSecondsHistogramBuckets(
        buckets: BucketBoundaries
    ): ClientMetricsConfig =
      copy(responseDurationSecondsHistogramBuckets = buckets)
  }
}
