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

import org.http4s.otel4s.middleware.client.UriTemplateClassifier
import org.http4s.otel4s.middleware.TypedAttributes
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries

/** Configuration for OpenTelemetry HTTP client metrics. */
sealed trait ClientMetricsConfig {

  /** Returns true when metrics should be recorded for a request. */
  def requestFilter: MetricsRequestFilter

  /** Attributes added to every enabled metric. */
  def additionalAttributes: Attributes

  /** Methods emitted verbatim as `http.request.method`; other methods are emitted as `_OTHER`. */
  def knownMethods: Set[Method]

  /** Explicit bucket boundaries, in seconds, for `http.client.request.duration`. */
  def requestDurationHistogramBuckets: BucketBoundaries

  /** Explicit bucket boundaries, in seconds, for
    * `http.client.response.headers.duration`.
    */
  def responseHeadersDurationHistogramBuckets: BucketBoundaries

  /** Explicit bucket boundaries, in bytes, for `http.client.request.body.size`. */
  def requestBodySizeHistogramBuckets: BucketBoundaries

  /** Explicit bucket boundaries, in bytes, for `http.client.response.body.size`. */
  def responseBodySizeHistogramBuckets: BucketBoundaries

  /** Whether the http4s-specific `http.client.response.headers.duration` metric is enabled. */
  def responseHeadersDurationEnabled: Boolean

  /** Whether the opt-in `http.client.active_requests` metric is enabled. */
  def activeRequestsEnabled: Boolean

  /** Whether the opt-in `http.client.request.body.size` metric is enabled. */
  def requestBodySizeEnabled: Boolean

  /** Whether the opt-in `http.client.response.body.size` metric is enabled. */
  def responseBodySizeEnabled: Boolean

  /** Whether the recommended `network.protocol.version` attribute is enabled. */
  def networkProtocolVersionEnabled: Boolean

  /** Whether the opt-in `url.scheme` attribute is enabled. */
  def urlSchemeEnabled: Boolean

  /** Classifies low-cardinality `url.template` values. */
  def urlTemplateClassifier: UriTemplateClassifier

  /** Replaces the attributes added to every enabled metric. */
  def withAdditionalAttributes(attributes: Attributes): ClientMetricsConfig

  /** Replaces the per-request metrics filter. Returning false drops all metrics for that request. */
  def withRequestFilter(filter: MetricsRequestFilter): ClientMetricsConfig

  /** Replaces the complete set of methods emitted verbatim as `http.request.method`. */
  def withKnownMethods(methods: Set[Method]): ClientMetricsConfig

  /** Replaces the `http.client.request.duration` histogram boundaries. */
  def withRequestDurationHistogramBuckets(
      buckets: BucketBoundaries
  ): ClientMetricsConfig

  /** Replaces the `http.client.response.headers.duration` histogram boundaries. */
  def withResponseHeadersDurationHistogramBuckets(
      buckets: BucketBoundaries
  ): ClientMetricsConfig

  /** Replaces the `http.client.request.body.size` histogram boundaries. */
  def withRequestBodySizeHistogramBuckets(
      buckets: BucketBoundaries
  ): ClientMetricsConfig

  /** Replaces the `http.client.response.body.size` histogram boundaries. */
  def withResponseBodySizeHistogramBuckets(
      buckets: BucketBoundaries
  ): ClientMetricsConfig

  /** Enables or disables the http4s-specific `http.client.response.headers.duration` metric. */
  def withResponseHeadersDuration(enabled: Boolean): ClientMetricsConfig

  /** Enables or disables the  `http.client.active_requests` metric. */
  def withActiveRequests(enabled: Boolean): ClientMetricsConfig

  /** Enables or disables `http.client.request.body.size` metric. */
  def withRequestBodySize(enabled: Boolean): ClientMetricsConfig

  /** Enables or disables the `http.client.response.body.size` metric. */
  def withResponseBodySize(enabled: Boolean): ClientMetricsConfig

  /** Enables or disables the recommended `network.protocol.version` attribute. */
  def withNetworkProtocolVersion(enabled: Boolean): ClientMetricsConfig

  /** Enables or disables the opt-in `url.scheme` attribute. */
  def withUrlScheme(enabled: Boolean): ClientMetricsConfig

  /** Replaces the classifier used to emit low-cardinality `url.template` values. */
  def withUrlTemplateClassifier(classifier: UriTemplateClassifier): ClientMetricsConfig
}

object ClientMetricsConfig {

  /** Minimal OpenTelemetry configuration.
    *
    * Enables the stable `http.client.request.duration` metric with required and
    * conditionally-required attributes: `http.request.method`, `server.address`, `server.port`,
    * `http.response.status_code`, and `error.type`. Recommended and opt-in attributes, development
    * metrics, and the http4s-specific response-header duration metric are disabled.
    */
  val minimal: ClientMetricsConfig = Impl(
    requestFilter = MetricsRequestFilter.all,
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
    urlSchemeEnabled = false,
    urlTemplateClassifier = UriTemplateClassifier.indeterminate,
  )

  /** Recommended OpenTelemetry configuration.
    *
    * Enables everything in [[minimal]] plus the recommended `network.protocol.version` attribute
    * on metrics recorded after the response protocol version is known. Opt-in attributes,
    * development metrics, and the http4s-specific response-header duration metric remain disabled.
    */
  val recommended: ClientMetricsConfig = minimal.withNetworkProtocolVersion(true)

  /** All supported OpenTelemetry and http4s metric features.
    *
    * Enables everything in [[recommended]], the `http.client.active_requests`,
    * `http.client.request.body.size`, and `http.client.response.body.size` development metrics,
    * the http4s-specific `http.client.response.headers.duration` metric, and the opt-in
    * `url.scheme` attribute. A `url.template` attribute is added only when
    * [[org.http4s.otel4s.middleware.client.UriTemplateClassifier]] is configured.
    */
  val all: ClientMetricsConfig =
    recommended
      .withResponseHeadersDuration(true)
      .withActiveRequests(true)
      .withRequestBodySize(true)
      .withResponseBodySize(true)
      .withUrlScheme(true)

  private final case class Impl(
      additionalAttributes: Attributes,
      requestFilter: MetricsRequestFilter,
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
      urlSchemeEnabled: Boolean,
      urlTemplateClassifier: UriTemplateClassifier,
  ) extends ClientMetricsConfig {
    def withAdditionalAttributes(attributes: Attributes): ClientMetricsConfig =
      copy(additionalAttributes = attributes)
    def withRequestFilter(filter: MetricsRequestFilter): ClientMetricsConfig =
      copy(requestFilter = filter)
    def withKnownMethods(methods: Set[Method]): ClientMetricsConfig =
      copy(knownMethods = methods)
    def withRequestDurationHistogramBuckets(
        buckets: BucketBoundaries
    ): ClientMetricsConfig = copy(requestDurationHistogramBuckets = buckets)
    def withResponseHeadersDurationHistogramBuckets(
        buckets: BucketBoundaries
    ): ClientMetricsConfig = copy(responseHeadersDurationHistogramBuckets = buckets)
    def withRequestBodySizeHistogramBuckets(
        buckets: BucketBoundaries
    ): ClientMetricsConfig = copy(requestBodySizeHistogramBuckets = buckets)
    def withResponseBodySizeHistogramBuckets(
        buckets: BucketBoundaries
    ): ClientMetricsConfig = copy(responseBodySizeHistogramBuckets = buckets)
    def withResponseHeadersDuration(enabled: Boolean): ClientMetricsConfig =
      copy(responseHeadersDurationEnabled = enabled)
    def withActiveRequests(enabled: Boolean): ClientMetricsConfig =
      copy(activeRequestsEnabled = enabled)
    def withRequestBodySize(enabled: Boolean): ClientMetricsConfig =
      copy(requestBodySizeEnabled = enabled)
    def withResponseBodySize(enabled: Boolean): ClientMetricsConfig =
      copy(responseBodySizeEnabled = enabled)
    def withNetworkProtocolVersion(enabled: Boolean): ClientMetricsConfig =
      copy(networkProtocolVersionEnabled = enabled)
    def withUrlScheme(enabled: Boolean): ClientMetricsConfig =
      copy(urlSchemeEnabled = enabled)
    def withUrlTemplateClassifier(classifier: UriTemplateClassifier): ClientMetricsConfig =
      copy(urlTemplateClassifier = classifier)
  }

}
