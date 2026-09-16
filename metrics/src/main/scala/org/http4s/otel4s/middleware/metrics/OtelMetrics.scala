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

import java.util.concurrent.TimeUnit

import cats.Applicative
import cats.Monad
import cats.syntax.all._
import org.http4s.headers.Forwarded
import org.http4s.headers.Host
import org.http4s.metrics.{MetricsOps2, MetricsRequest, TerminationType}
import org.http4s.otel4s.middleware.client.TypedClientAttributes
import org.http4s.otel4s.middleware.server.OriginalScheme
import org.http4s.otel4s.middleware.server.TypedServerAttributes
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics._
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes

import scala.concurrent.duration.FiniteDuration

/** Creates [[http4s.metrics.MetricsOps2]] implementations that record HTTP client and server
  * metrics with OpenTelemetry.
  *
  * The following metrics are supported for both clients and servers, where `*` is replaced by
  * `client` or `server`:
  *
  *   - `http.*.request.duration` (`Histogram[Double]`, seconds) records the total time from the
  *     beginning of a request until its response body terminates. This stable OpenTelemetry metric
  *     is always enabled.
  *   - `http.*.active_requests` (`UpDownCounter[Long]`, requests) records requests currently in
  *     progress. This development OpenTelemetry metric is opt-in.
  *   - `http.*.request.body.size` (`Histogram[Long]`, bytes) records the number of request-body
  *     bytes consumed by the middleware. This development OpenTelemetry metric is opt-in.
  *   - `http.*.response.body.size` (`Histogram[Long]`, bytes) records the number of response-body
  *     bytes consumed by the middleware. This development OpenTelemetry metric is opt-in.
  *   - `http.*.response.headers.duration` (`Histogram[Double]`, seconds) records the time until
  *     response headers are received by a client or sent by a server. This is an http4s-specific,
  *     non-semantic-convention metric and is opt-in.
  *
  * [[ClientMetricsConfig]] and [[ServerMetricsConfig]] provide three presets:
  *
  *   - `minimal` emits request duration with required and conditionally-required attributes.
  *   - `recommended` additionally emits attributes recommended by OpenTelemetry. It is the default
  *     preset.
  *   - `all` additionally enables every supported opt-in metric and attribute. Classifier-derived
  *     attributes are still emitted only when the configured classifier returns a value.
  *
  * Additional attributes configured by the user are added to every enabled metric. Semantic
  * attributes derived from the HTTP request or response take precedence when an attribute key is
  * present in both sets. Histogram bucket settings affect only their corresponding histogram.
  *
  * Body sizes are measured from the streams visible to the http4s metrics middleware. To record
  * the transport-encoded sizes required by OpenTelemetry, place client metrics inside response
  * decompression middleware, close to the underlying client; for example,
  * `GZip()(Metrics(ops)(client))`. Place server metrics outside response compression middleware,
  * close to the server transport; for example, `Metrics(ops)(GZip(routes))`. Reversing either
  * ordering records application-facing, decoded body sizes instead. The same principle applies to
  * middleware that transforms request bodies: metrics must observe the encoded side of the
  * transformation.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-metrics/ OpenTelemetry HTTP metric semantic conventions]]
  */
object OtelMetrics {

  /** Creates HTTP client metrics using the given configuration.
    *
    * Client request duration and body-size metrics include `http.request.method`, `server.address`,
    * and `server.port` when those values can be derived. `http.response.status_code`,
    * `error.type`, and `network.protocol.version` are added after termination when applicable.
    * `url.scheme` and `url.template` are controlled by [[ClientMetricsConfig]].
    *
    * @param config controls enabled metrics, semantic attributes, classifiers, additional
    *               attributes, and histogram boundaries
    */
  def clientMetricsOps[F[_]: Monad: MeterProvider](
      config: ClientMetricsConfig
  ): F[MetricsOps2[F]] =
    metricsOps(
      kind = "client",
      config.requestDurationHistogramBuckets,
      config.responseHeadersDurationHistogramBuckets,
      config.requestBodySizeHistogramBuckets,
      config.responseBodySizeHistogramBuckets,
      config.responseHeadersDurationEnabled,
      config.activeRequestsEnabled,
      config.requestBodySizeEnabled,
      config.responseBodySizeEnabled,
      clientContext(config),
    )

  /** Creates HTTP client metrics using the original argument-based API.
    *
    * This overload enables active requests, request and response body sizes, and the
    * http4s-specific response-header duration metric to preserve the behavior of the
    * argument-based API. Use the [[ClientMetricsConfig]] overload to select a preset or opt into
    * individual instruments and attributes.
    *
    * @param attributes attributes added to every emitted metric
    * @param responseDurationSecondsHistogramBuckets boundaries used by both request-duration and
    *                                                response-header-duration histograms; body-size
    *                                                histograms use their byte-oriented defaults
    */
  def clientMetricsOps[F[_]: Monad: MeterProvider](
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries =
        MetricsConfigDefaults.DurationHistogramBuckets,
  ): F[MetricsOps2[F]] =
    clientMetricsOps(
      ClientMetricsConfig.recommended
        .withAdditionalAttributes(attributes)
        .withRequestDurationHistogramBuckets(responseDurationSecondsHistogramBuckets)
        .withResponseHeadersDurationHistogramBuckets(responseDurationSecondsHistogramBuckets)
        .withResponseHeadersDuration(true)
        .withActiveRequests(true)
        .withRequestBodySize(true)
        .withResponseBodySize(true)
    )

  /** Creates HTTP server metrics using the given configuration.
    *
    * Server request duration and body-size metrics include `http.request.method` and `url.scheme`
    * when those values can be derived. `http.response.status_code` and `error.type` are added after
    * termination when applicable. `network.protocol.version`, `http.route`, `server.address`, and
    * `server.port` are controlled by [[ServerMetricsConfig]]. Server address and port are disabled
    * by default because request-header-derived values may have unbounded cardinality.
    *
    * @param config controls enabled metrics, semantic attributes, classifiers, additional
    *               attributes, and histogram boundaries
    */
  def serverMetricsOps[F[_]: Monad: MeterProvider](
      config: ServerMetricsConfig
  ): F[MetricsOps2[F]] =
    metricsOps(
      kind = "server",
      config.requestDurationHistogramBuckets,
      config.responseHeadersDurationHistogramBuckets,
      config.requestBodySizeHistogramBuckets,
      config.responseBodySizeHistogramBuckets,
      config.responseHeadersDurationEnabled,
      config.activeRequestsEnabled,
      config.requestBodySizeEnabled,
      config.responseBodySizeEnabled,
      serverContext(config),
    )

  /** Creates HTTP server metrics using the original argument-based API.
    *
    * This overload enables active requests, request and response body sizes, and the
    * http4s-specific response-header duration metric to preserve the behavior of the
    * argument-based API. Server address and port remain disabled because they are OpenTelemetry
    * opt-in attributes. Use the [[ServerMetricsConfig]] overload to select a preset or opt into
    * individual instruments and attributes.
    *
    * @param attributes attributes added to every emitted metric
    * @param responseDurationSecondsHistogramBuckets boundaries used by both request-duration and
    *                                                response-header-duration histograms; body-size
    *                                                histograms use their byte-oriented defaults
    */
  def serverMetricsOps[F[_]: Monad: MeterProvider](
      attributes: Attributes = Attributes.empty,
      responseDurationSecondsHistogramBuckets: BucketBoundaries =
        MetricsConfigDefaults.DurationHistogramBuckets,
  ): F[MetricsOps2[F]] =
    serverMetricsOps(
      ServerMetricsConfig.recommended
        .withAdditionalAttributes(attributes)
        .withRequestDurationHistogramBuckets(responseDurationSecondsHistogramBuckets)
        .withResponseHeadersDurationHistogramBuckets(responseDurationSecondsHistogramBuckets)
        .withResponseHeadersDuration(true)
        .withActiveRequests(true)
        .withRequestBodySize(true)
        .withResponseBodySize(true)
    )

  private final case class MetricsContext(
      activeRequestAttributes: Attributes,
      requestAttributes: Attributes,
      responseProtocolVersionEnabled: Boolean,
  )

  private def clientContext(config: ClientMetricsConfig)(
      metricsRequest: MetricsRequest
  ): Option[MetricsContext] = Option.when(config.requestFilter(metricsRequest)) {
    val request = metricsRequest.requestPrelude
    val hostHeader = request.headers.get[Host]

    val common = config.additionalAttributes
      .added(TypedClientAttributes.httpRequestMethod(request.method, config.knownMethods))
      .concat(
        request.uri.host
          .map(TypedClientAttributes.serverAddress)
          .orElse(hostHeader.map(host => TypedClientAttributes.serverAddress(host.host)))
      )
      // `Request#remote` is the immediate peer, not necessarily the origin server.
      .concat(
        TypedClientAttributes
          .serverPort(None, request.uri)
          .orElse(hostHeader.flatMap(_.port.map(TypedClientAttributes.serverPort)))
      )

    val optIn = Attributes.newBuilder
    if (config.urlSchemeEnabled)
      optIn ++= TypedClientAttributes.urlScheme(request.uri.scheme)
    optIn ++= TypedClientAttributes.Experimental
      .urlTemplate(request.uri, config.urlTemplateClassifier)

    val configured = common ++ optIn.result()
    MetricsContext(
      activeRequestAttributes = configured,
      requestAttributes = configured,
      responseProtocolVersionEnabled = config.networkProtocolVersionEnabled,
    )
  }

  private def serverContext(config: ServerMetricsConfig)(
      metricsRequest: MetricsRequest
  ): Option[MetricsContext] = Option.when(config.requestFilter(metricsRequest)) {
    val request = metricsRequest.requestPrelude

    val forwarded = request.headers.get[Forwarded]
    val scheme = OriginalScheme(
      forwarded,
      request.headers,
      request.uri,
      metricsRequest.connectionInfo.map(_.secure),
    )
    val commonBuilder = Attributes.newBuilder
    commonBuilder ++= config.additionalAttributes
    commonBuilder += TypedServerAttributes.httpRequestMethod(request.method, config.knownMethods)
    commonBuilder ++= TypedServerAttributes.urlScheme(scheme)
    if (config.serverAddressAndPortEnabled)
      TypedServerAttributes.serverAddressAndPortForBuilder(request, forwarded, scheme)(
        commonBuilder
      )

    val common = commonBuilder.result()
    val recommended =
      if (config.networkProtocolVersionEnabled)
        common.added(TypedServerAttributes.networkProtocolVersion(request.httpVersion))
      else common

    MetricsContext(
      activeRequestAttributes = common,
      requestAttributes = recommended
        .concat(TypedServerAttributes.httpRoute(request, config.routeClassifier)),
      responseProtocolVersionEnabled = false,
    )
  }

  private def metricsOps[F[_]: Monad: MeterProvider](
      kind: String,
      requestDurationHistogramBuckets: BucketBoundaries,
      responseHeadersDurationHistogramBuckets: BucketBoundaries,
      requestBodySizeHistogramBuckets: BucketBoundaries,
      responseBodySizeHistogramBuckets: BucketBoundaries,
      responseHeadersDurationEnabled: Boolean,
      activeRequestsEnabled: Boolean,
      requestBodySizeEnabled: Boolean,
      responseBodySizeEnabled: Boolean,
      context: MetricsRequest => Option[MetricsContext],
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
          requestDurationHistogramBuckets,
          responseHeadersDurationHistogramBuckets,
          requestBodySizeHistogramBuckets,
          responseBodySizeHistogramBuckets,
          responseHeadersDurationEnabled,
          activeRequestsEnabled,
          requestBodySizeEnabled,
          responseBodySizeEnabled,
        )
      }
    } yield createMetricsOps(metrics, context, kind)

  private def createMetricsOps[F[_]: Applicative](
      metrics: MetricsCollection[F],
      createRequestContext: MetricsRequest => Option[MetricsContext],
      kind: String,
  ): MetricsOps2[F] =
    new MetricsOps2[F] {
      type Context = MetricsContext

      override def createContext(request: MetricsRequest): F[Option[MetricsContext]] =
        createRequestContext(request).pure[F]

      override def increaseActiveRequests(
          request: MetricsRequest,
          context: MetricsContext,
      ): F[Unit] =
        metrics.activeRequests.traverse_(_.inc(context.activeRequestAttributes))

      override def decreaseActiveRequests(
          request: MetricsRequest,
          context: MetricsContext,
      ): F[Unit] =
        metrics.activeRequests.traverse_(_.dec(context.activeRequestAttributes))

      override def recordHeadersTime(
          request: MetricsRequest,
          elapsed: FiniteDuration,
          context: MetricsContext,
      ): F[Unit] =
        metrics.responseHeadersDuration.traverse_(
          _.record(
            elapsed.toUnit(TimeUnit.SECONDS),
            context.requestAttributes,
          )
        )

      override def recordTotalTime(
          request: MetricsRequest,
          response: Option[ResponsePrelude],
          terminationType: Option[TerminationType],
          elapsed: FiniteDuration,
          context: MetricsContext,
      ): F[Unit] =
        metrics.requestDuration.record(
          elapsed.toUnit(TimeUnit.SECONDS),
          finalAttributes(response, terminationType, context),
        )

      override def recordRequestBodySize(
          request: MetricsRequest,
          response: Option[ResponsePrelude],
          terminationType: Option[TerminationType],
          bodySizeBytes: Long,
          context: MetricsContext,
      ): F[Unit] =
        metrics.requestBodySize.traverse_(
          _.record(bodySizeBytes, finalAttributes(response, terminationType, context))
        )

      override def recordResponseBodySize(
          request: MetricsRequest,
          response: ResponsePrelude,
          terminationType: Option[TerminationType],
          bodySizeBytes: Long,
          context: MetricsContext,
      ): F[Unit] =
        metrics.responseBodySize.traverse_(
          _.record(bodySizeBytes, finalAttributes(Some(response), terminationType, context))
        )

      private def finalAttributes(
          response: Option[ResponsePrelude],
          terminationType: Option[TerminationType],
          context: MetricsContext,
      ): Attributes =
        context.requestAttributes
          .concat(
            response
              .filter(_ => context.responseProtocolVersionEnabled)
              .map(r => TypedAttributes.networkProtocolVersion(r.httpVersion))
          )
          .concat(TypedAttributes.httpResponseStatusCode(response.map(_.status)))
          .concat(TypedMetricAttributes.errorType(kind, response, terminationType))
    }

  private def createMetricsCollection[F[_]: Monad: Meter](
      kind: String,
      requestDurationHistogramBuckets: BucketBoundaries,
      responseHeadersDurationHistogramBuckets: BucketBoundaries,
      requestBodySizeHistogramBuckets: BucketBoundaries,
      responseBodySizeHistogramBuckets: BucketBoundaries,
      responseHeadersDurationEnabled: Boolean,
      activeRequestsEnabled: Boolean,
      requestBodySizeEnabled: Boolean,
      responseBodySizeEnabled: Boolean,
  ): F[MetricsCollection[F]] = {
    val requestDuration = Meter[F]
      .histogram[Double](s"http.$kind.request.duration")
      .withUnit("s")
      .withDescription(s"Duration of HTTP $kind requests.")
      .withExplicitBucketBoundaries(requestDurationHistogramBuckets)
      .create

    val responseHeadersDuration =
      Option
        .when(responseHeadersDurationEnabled)(
          Meter[F]
            .histogram[Double](s"http.$kind.response.headers.duration")
            .withUnit("s")
            .withDescription(
              if (kind == "client") "Time to receive HTTP client response headers."
              else "Time to send HTTP server response headers."
            )
            .withExplicitBucketBoundaries(responseHeadersDurationHistogramBuckets)
            .create
        )
        .sequence

    val activeRequests =
      Option
        .when(activeRequestsEnabled)(
          Meter[F]
            .upDownCounter[Long](s"http.$kind.active_requests")
            .withUnit("{request}")
            .withDescription(
              s"Number of active HTTP ${if (kind == "client") "" else "server "}requests."
            )
            .create
        )
        .sequence

    val requestBodySize =
      Option
        .when(requestBodySizeEnabled)(
          Meter[F]
            .histogram[Long](s"http.$kind.request.body.size")
            .withUnit("By")
            .withDescription(s"Size of HTTP $kind request bodies.")
            .withExplicitBucketBoundaries(requestBodySizeHistogramBuckets)
            .create
        )
        .sequence

    val responseBodySize =
      Option
        .when(responseBodySizeEnabled)(
          Meter[F]
            .histogram[Long](s"http.$kind.response.body.size")
            .withUnit("By")
            .withDescription(s"Size of HTTP $kind response bodies.")
            .withExplicitBucketBoundaries(responseBodySizeHistogramBuckets)
            .create
        )
        .sequence

    (
      requestDuration,
      responseHeadersDuration,
      activeRequests,
      requestBodySize,
      responseBodySize,
    ).mapN(MetricsCollection.apply)
  }

  final case class MetricsCollection[F[_]](
      requestDuration: Histogram[F, Double],
      responseHeadersDuration: Option[Histogram[F, Double]],
      activeRequests: Option[UpDownCounter[F, Long]],
      requestBodySize: Option[Histogram[F, Long]],
      responseBodySize: Option[Histogram[F, Long]],
  )

  private object TypedMetricAttributes {
    def errorType(
        kind: String,
        response: Option[ResponsePrelude],
        terminationType: Option[TerminationType],
    ): Option[Attribute[String]] =
      ErrorAttributes.ErrorType.maybe(
        terminationType
          .map {
            case TerminationType.Abnormal(e) => e.getClass.getName
            case TerminationType.Error(e) => e.getClass.getName
            case TerminationType.Canceled => "cancel"
            case TerminationType.Timeout => "timeout"
          }
          .orElse(
            response
              .map(_.status)
              .filter { status =>
                status.responseClass == Status.ServerError ||
                (kind == "client" && status.responseClass == Status.ClientError)
              }
              .map(_.code.toString)
          )
      )
  }
}
