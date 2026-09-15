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

import org.http4s.otel4s.middleware.server.RouteClassifier
import org.http4s.otel4s.middleware.TypedAttributes
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries

/** Configuration for OpenTelemetry HTTP server metrics. */
sealed trait ServerMetricsConfig {

  /** Attributes added to every enabled metric. */
  def additionalAttributes: Attributes

  /** Methods emitted verbatim as `http.request.method`; other methods are emitted as `_OTHER`. */
  def knownMethods: Set[Method]

  /** Explicit bucket boundaries, in seconds, for `http.server.request.duration`. */
  def requestDurationHistogramBuckets: BucketBoundaries

  /** Explicit bucket boundaries, in seconds, for
    * `http.server.response.headers.duration`.
    */
  def responseHeadersDurationHistogramBuckets: BucketBoundaries

  /** Explicit bucket boundaries, in bytes, for `http.server.request.body.size`. */
  def requestBodySizeHistogramBuckets: BucketBoundaries

  /** Explicit bucket boundaries, in bytes, for `http.server.response.body.size`. */
  def responseBodySizeHistogramBuckets: BucketBoundaries

  /** Whether the http4s-specific `http.server.response.headers.duration` metric is enabled. */
  def responseHeadersDurationEnabled: Boolean

  /** Whether the opt-in `http.server.active_requests` metric is enabled. */
  def activeRequestsEnabled: Boolean

  /** Whether the opt-in `http.server.request.body.size` metric is enabled. */
  def requestBodySizeEnabled: Boolean

  /** Whether the opt-in `http.server.response.body.size` metric is enabled. */
  def responseBodySizeEnabled: Boolean

  /** Whether the recommended `network.protocol.version` attribute is enabled. */
  def networkProtocolVersionEnabled: Boolean

  /** Classifies low-cardinality `http.route` values. */
  def routeClassifier: RouteClassifier

  /** Whether the opt-in `server.address` and `server.port` attributes are enabled. */
  def serverAddressAndPortEnabled: Boolean

  /** Replaces the attributes added to every enabled metric. */
  def withAdditionalAttributes(attributes: Attributes): ServerMetricsConfig

  /** Replaces the complete set of methods emitted verbatim as `http.request.method`. */
  def withKnownMethods(methods: Set[Method]): ServerMetricsConfig

  /** Replaces the `http.server.request.duration` histogram boundaries. */
  def withRequestDurationHistogramBuckets(
      buckets: BucketBoundaries
  ): ServerMetricsConfig

  /** Replaces the `http.server.response.headers.duration` histogram boundaries. */
  def withResponseHeadersDurationHistogramBuckets(
      buckets: BucketBoundaries
  ): ServerMetricsConfig

  /** Replaces the `http.server.request.body.size` histogram boundaries. */
  def withRequestBodySizeHistogramBuckets(
      buckets: BucketBoundaries
  ): ServerMetricsConfig

  /** Replaces the `http.server.response.body.size` histogram boundaries. */
  def withResponseBodySizeHistogramBuckets(
      buckets: BucketBoundaries
  ): ServerMetricsConfig

  /** Enables the http4s-specific `http.server.response.headers.duration` metric. */
  def optIntoResponseHeadersDuration: ServerMetricsConfig

  /** Enables the opt-in `http.server.active_requests` metric. */
  def optIntoActiveRequests: ServerMetricsConfig

  /** Enables the opt-in `http.server.request.body.size` metric. */
  def optIntoRequestBodySize: ServerMetricsConfig

  /** Enables the opt-in `http.server.response.body.size` metric. */
  def optIntoResponseBodySize: ServerMetricsConfig

  /** Enables the recommended `network.protocol.version` attribute. */
  def withNetworkProtocolVersion: ServerMetricsConfig

  /** Replaces the classifier used to emit low-cardinality `http.route` values. */
  def withRouteClassifier(classifier: RouteClassifier): ServerMetricsConfig

  /** Enables the opt-in `server.address` and `server.port` attributes. */
  def optIntoServerAddressAndPort: ServerMetricsConfig
}

object ServerMetricsConfig {

  /** Minimal OpenTelemetry configuration.
    *
    * Enables the stable `http.server.request.duration` metric with required and
    * conditionally-required attributes: `http.request.method`, `url.scheme`,
    * `http.response.status_code`, and `error.type`. Recommended and opt-in attributes, development
    * metrics, the `http.route` classifier, and the http4s-specific response-header duration metric
    * are disabled.
    */
  val minimal: ServerMetricsConfig = Impl(
    additionalAttributes = Attributes.empty,
    knownMethods = TypedAttributes.defaultKnownMethods,
    requestDurationHistogramBuckets = MetricsConfigDefaults.DurationHistogramBuckets,
    responseHeadersDurationHistogramBuckets = MetricsConfigDefaults.DurationHistogramBuckets,
    requestBodySizeHistogramBuckets = MetricsConfigDefaults.BodySizeHistogramBuckets,
    responseBodySizeHistogramBuckets = MetricsConfigDefaults.BodySizeHistogramBuckets,
    responseHeadersDurationEnabled = false,
    activeRequestsEnabled = false,
    requestBodySizeEnabled = false,
    responseBodySizeEnabled = false,
    networkProtocolVersionEnabled = false,
    routeClassifier = RouteClassifier.indeterminate,
    serverAddressAndPortEnabled = false,
  )

  /** Recommended OpenTelemetry configuration.
    *
    * Enables everything in [[minimal]] plus the recommended `network.protocol.version` attribute.
    * Opt-in attributes, development metrics, the `http.route` classifier, and the http4s-specific
    * response-header duration metric remain disabled.
    */
  val recommended: ServerMetricsConfig = minimal.withNetworkProtocolVersion

  /** All supported OpenTelemetry and http4s metric features.
    *
    * Enables everything in [[recommended]], the `http.server.active_requests`,
    * `http.server.request.body.size`, and `http.server.response.body.size` development metrics,
    * the http4s-specific `http.server.response.headers.duration` metric, and the opt-in
    * `server.address` and `server.port` attributes. An `http.route` attribute is added only when
    * [[org.http4s.otel4s.middleware.server.RouteClassifier]] is configured.
    */
  val all: ServerMetricsConfig =
    recommended.optIntoResponseHeadersDuration.optIntoActiveRequests.optIntoRequestBodySize.optIntoResponseBodySize.optIntoServerAddressAndPort

  private final case class Impl(
      additionalAttributes: Attributes,
      knownMethods: Set[Method],
      requestDurationHistogramBuckets: BucketBoundaries,
      responseHeadersDurationHistogramBuckets: BucketBoundaries,
      requestBodySizeHistogramBuckets: BucketBoundaries,
      responseBodySizeHistogramBuckets: BucketBoundaries,
      responseHeadersDurationEnabled: Boolean,
      activeRequestsEnabled: Boolean,
      requestBodySizeEnabled: Boolean,
      responseBodySizeEnabled: Boolean,
      networkProtocolVersionEnabled: Boolean,
      routeClassifier: RouteClassifier,
      serverAddressAndPortEnabled: Boolean,
  ) extends ServerMetricsConfig {
    def withAdditionalAttributes(attributes: Attributes): ServerMetricsConfig =
      copy(additionalAttributes = attributes)
    def withKnownMethods(methods: Set[Method]): ServerMetricsConfig =
      copy(knownMethods = methods)
    def withRequestDurationHistogramBuckets(
        buckets: BucketBoundaries
    ): ServerMetricsConfig = copy(requestDurationHistogramBuckets = buckets)
    def withResponseHeadersDurationHistogramBuckets(
        buckets: BucketBoundaries
    ): ServerMetricsConfig = copy(responseHeadersDurationHistogramBuckets = buckets)
    def withRequestBodySizeHistogramBuckets(
        buckets: BucketBoundaries
    ): ServerMetricsConfig = copy(requestBodySizeHistogramBuckets = buckets)
    def withResponseBodySizeHistogramBuckets(
        buckets: BucketBoundaries
    ): ServerMetricsConfig = copy(responseBodySizeHistogramBuckets = buckets)
    def optIntoResponseHeadersDuration: ServerMetricsConfig =
      copy(responseHeadersDurationEnabled = true)
    def optIntoActiveRequests: ServerMetricsConfig = copy(activeRequestsEnabled = true)
    def optIntoRequestBodySize: ServerMetricsConfig = copy(requestBodySizeEnabled = true)
    def optIntoResponseBodySize: ServerMetricsConfig = copy(responseBodySizeEnabled = true)
    def withNetworkProtocolVersion: ServerMetricsConfig =
      copy(networkProtocolVersionEnabled = true)
    def withRouteClassifier(classifier: RouteClassifier): ServerMetricsConfig =
      copy(routeClassifier = classifier)
    def optIntoServerAddressAndPort: ServerMetricsConfig =
      copy(serverAddressAndPortEnabled = true)
  }

}
