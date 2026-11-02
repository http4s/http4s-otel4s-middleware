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

import org.http4s.RequestPrelude
import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.MetricsDefaults
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries

/** Immutable server metrics settings. */
sealed trait ServerMetricsConfig {
  def attributes: Attributes
  def requestAttributes: RequestPrelude => Attributes
  def requestFilter: PerRequestFilter
  def requestBodySize: Option[BodySizeConfig]
  def responseBodySize: Option[BodySizeConfig]
  def errorClassifier: ErrorClassifier
  def routeClassifier: RouteClassifier
  def activeRequestsEnabled: Boolean
  def serverAddressEnabled: Boolean
  def serverPortEnabled: Boolean
  def responseDurationSecondsHistogramBuckets: BucketBoundaries

  /** Sets shared labels. */
  def withAttributes(attributes: Attributes): ServerMetricsConfig

  /** Sets low-cardinality request labels. Automatic HTTP attributes take precedence. */
  def withRequestAttributes(attributes: RequestPrelude => Attributes): ServerMetricsConfig

  /** Checks once per invocation; excluded requests skip attribute callbacks. */
  def withRequestFilter(filter: PerRequestFilter): ServerMetricsConfig

  /** Classifies error responses. Defaults to 5xx; observed exceptions take precedence. */
  def withErrorClassifier(classifier: ErrorClassifier): ServerMetricsConfig

  /** Enables request-size recording from Content-Length with default byte buckets. */
  def optIntoRequestBodySize: ServerMetricsConfig

  /** Enables request-size recording with the selected source and byte buckets. */
  def optIntoRequestBodySize(config: BodySizeConfig): ServerMetricsConfig

  /** Uses Content-Length or protocol-known zero for response size, with default byte buckets. */
  def optIntoResponseBodySize: ServerMetricsConfig

  /** Enables response-size recording with the selected source and byte buckets. */
  def optIntoResponseBodySize(config: BodySizeConfig): ServerMetricsConfig

  /** Enables the experimental `http.server.active_requests` metric (disabled by default). */
  def optIntoActiveRequests: ServerMetricsConfig

  /** Adds `server.address` using the existing forwarded/authority/Host extraction. */
  def optIntoServerAddress: ServerMetricsConfig

  /** Adds `server.port` using the existing forwarded/authority/Host extraction. */
  def optIntoServerPort: ServerMetricsConfig

  /** Adds a classified `http.route` to completion histograms. No raw-path fallback. */
  def withRouteClassifier(classifier: RouteClassifier): ServerMetricsConfig

  /** Sets duration histogram buckets in seconds. */
  def withResponseDurationSecondsHistogramBuckets(buckets: BucketBoundaries): ServerMetricsConfig
}

object ServerMetricsConfig {

  /** Records duration for all requests with standard buckets; 5xx responses are errors.
    * Active requests, body sizes, custom labels, route classification, and server address/port are disabled.
    */
  val default: ServerMetricsConfig = Impl()

  private final case class Impl(
      attributes: Attributes = Attributes.empty,
      requestAttributes: RequestPrelude => Attributes = _ => Attributes.empty,
      requestFilter: PerRequestFilter = PerRequestFilter.alwaysEnabled,
      errorClassifier: ErrorClassifier = ErrorClassifier.serverError,
      routeClassifier: RouteClassifier = RouteClassifier.indeterminate,
      requestBodySize: Option[BodySizeConfig] = None,
      responseBodySize: Option[BodySizeConfig] = None,
      activeRequestsEnabled: Boolean = false,
      serverAddressEnabled: Boolean = false,
      serverPortEnabled: Boolean = false,
      responseDurationSecondsHistogramBuckets: BucketBoundaries = MetricsDefaults.DurationBuckets,
  ) extends ServerMetricsConfig {
    def withAttributes(attributes: Attributes): ServerMetricsConfig = copy(attributes = attributes)
    def withRequestAttributes(attributes: RequestPrelude => Attributes): ServerMetricsConfig =
      copy(requestAttributes = attributes)
    def withRequestFilter(filter: PerRequestFilter): ServerMetricsConfig =
      copy(requestFilter = filter)
    def withErrorClassifier(classifier: ErrorClassifier): ServerMetricsConfig =
      copy(errorClassifier = classifier)
    def optIntoRequestBodySize: ServerMetricsConfig =
      optIntoRequestBodySize(BodySizeConfig.contentLength)

    def optIntoRequestBodySize(config: BodySizeConfig): ServerMetricsConfig =
      copy(requestBodySize = Some(config))

    def optIntoResponseBodySize: ServerMetricsConfig =
      optIntoResponseBodySize(BodySizeConfig.contentLength)

    def optIntoResponseBodySize(config: BodySizeConfig): ServerMetricsConfig =
      copy(responseBodySize = Some(config))

    def optIntoActiveRequests: ServerMetricsConfig = copy(activeRequestsEnabled = true)
    def withRouteClassifier(classifier: RouteClassifier): ServerMetricsConfig =
      copy(routeClassifier = classifier)
    def optIntoServerAddress: ServerMetricsConfig = copy(serverAddressEnabled = true)
    def optIntoServerPort: ServerMetricsConfig = copy(serverPortEnabled = true)
    def withResponseDurationSecondsHistogramBuckets(
        buckets: BucketBoundaries
    ): ServerMetricsConfig =
      copy(responseDurationSecondsHistogramBuckets = buckets)
  }
}
