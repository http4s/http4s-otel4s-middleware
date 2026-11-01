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

import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.syntax.all._
import cats.syntax.all._
import cats.~>
import org.http4s.Method
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.ResponsePrelude
import org.http4s.Status
import org.http4s.otel4s.middleware.BuildInfo
import org.http4s.otel4s.middleware.client.TypedClientAttributes
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.metrics.Histogram
import org.typelevel.otel4s.metrics.UpDownCounter
import org.typelevel.otel4s.semconv.SchemaUrls

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** Instruments and per-exchange recording, independent of completion ownership. */
private[middleware] final class ClientMetricsRecorder[F[_]: Temporal] private[client] (
    otel4s: Otel4s[F],
    config: ClientMetricsConfig,
    duration: Histogram[F, Double],
    active: Option[UpDownCounter[F, Long]],
    requestBodySize: Option[Histogram[F, Long]],
    responseBodySize: Option[Histogram[F, Long]],
) {
  private val optionalInstrumentMetadata = List(
    active.map(_.backend.meta),
    requestBodySize.map(_.backend.meta),
    responseBodySize.map(_.backend.meta),
  ).flatten

  // Instrument metadata may change, so check it for each acquisition.
  def isEnabled: F[Boolean] =
    duration.backend.meta.isEnabled.ifM(
      true.pure[F],
      optionalInstrumentMetadata.existsM(_.isEnabled),
    )

  def start(
      request: Request[F],
      prelude: RequestPrelude,
  ): F[ClientMetricsRecorder.Exchange[F]] = {
    val startAttributes = {
      val builder = Attributes.newBuilder
      builder ++= config.attributes
      builder ++= config.requestAttributes(prelude)
      builder += TypedClientAttributes.httpRequestMethod(prelude.method)
      builder ++= TypedClientAttributes.serverAddress(prelude.uri.host)
      builder ++= TypedClientAttributes.serverPort(request.remotePort, prelude.uri)

      if (config.urlSchemeEnabled)
        builder ++= TypedClientAttributes.urlScheme(prelude.uri.scheme)

      builder ++= TypedClientAttributes.Experimental.urlTemplate(
        prelude.uri,
        config.urlTemplateClassifier,
      )

      builder.result()
    }

    for {
      context <- otel4s.localContext.ask[otel4s.Ctx]
      responseData <- Ref.of[F, Option[ClientMetricsRecorder.ResponseData]](None)
      requestSize <- BodySizeRecording.create(
        config.requestBodySize,
        requestBodySize,
        prelude.headers,
      )
      responseSize <- BodySizeRecording.create(config.responseBodySize, responseBodySize)
      runInContext = new (F ~> F) {
        def apply[A](fa: F[A]): F[A] = otel4s.localContext.scope(fa)(context)
      }
      exchange = new ClientMetricsRecorder.Exchange(
        responseData,
        startAttributes,
        runInContext,
        duration,
        active,
        requestSize,
        responseSize,
        config,
      )
      _ <- active.traverse_(_.inc(startAttributes))
    } yield exchange
  }
}

private[middleware] object ClientMetricsRecorder {

  def create[F[_]: Temporal](
      otel4s: Otel4s[F],
      config: ClientMetricsConfig,
  ): F[ClientMetricsRecorder[F]] =
    for {
      meter <- otel4s.meterProvider
        .meter("org.http4s.otel4s.middleware.client")
        .withVersion(BuildInfo.version)
        .withSchemaUrl(SchemaUrls.V1_43_0)
        .get
      duration <- meter
        .histogram[Double]("http.client.request.duration")
        .withUnit("s")
        .withDescription("Duration of HTTP client requests.")
        .withExplicitBucketBoundaries(config.responseDurationSecondsHistogramBuckets)
        .create
      active <-
        if (config.activeRequestsEnabled)
          meter
            .upDownCounter[Long]("http.client.active_requests")
            .withUnit("{request}")
            .withDescription("Number of active HTTP requests.")
            .create
            .map(_.some)
        else none[UpDownCounter[F, Long]].pure[F]
      requestBodySize <- config.requestBodySize.traverse { sizeConfig =>
        meter
          .histogram[Long]("http.client.request.body.size")
          .withUnit("By")
          .withDescription("Size of HTTP client request bodies.")
          .withExplicitBucketBoundaries(sizeConfig.bucketBoundaries)
          .create
      }
      responseBodySize <- config.responseBodySize.traverse { sizeConfig =>
        meter
          .histogram[Long]("http.client.response.body.size")
          .withUnit("By")
          .withDescription("Size of HTTP client response bodies.")
          .withExplicitBucketBoundaries(sizeConfig.bucketBoundaries)
          .create
      }
    } yield new ClientMetricsRecorder(
      otel4s,
      config,
      duration,
      active,
      requestBodySize,
      responseBodySize,
    )

  private def completionAttributes(
      request: RequestPrelude,
      response: ResponsePrelude,
      config: ClientMetricsConfig,
  ): Attributes = {
    val base = Attributes(
      TypedClientAttributes.httpResponseStatusCode(response.status),
      TypedClientAttributes.networkProtocolVersion(response.httpVersion),
    )
    if (config.errorClassifier.isError(request, response))
      base.added(TypedClientAttributes.errorType(response.status))
    else base
  }

  private final case class ResponseData(attributes: Attributes, knownBodySize: Option[Long])

  /** Holds metric attributes and recording context, without owning completion.
    * The middleware's Lifecycle calls `record` once in its uncancelable completion.
    */
  final class Exchange[F[_]: Temporal] private[ClientMetricsRecorder] (
      responseData: Ref[F, Option[ResponseData]],
      startAttributes: Attributes,
      runInContext: F ~> F,
      duration: Histogram[F, Double],
      active: Option[UpDownCounter[F, Long]],
      requestBodySize: Option[BodySizeRecording[F]],
      responseBodySize: Option[BodySizeRecording[F]],
      config: ClientMetricsConfig,
  ) {
    // Publishing the response snapshot transfers completion to the body/response-release finalizers.
    // Until then, the outer acquisition finalizer records acquisition failure or cancellation.
    def hasResponse: F[Boolean] = responseData.get.map(_.nonEmpty)

    def record(elapsed: FiniteDuration, error: Option[Throwable]): F[Unit] =
      for {
        requestSize <- requestBodySize.fold(none[Long].pure[F])(_.freeze())
        response <- responseData.get
        responseSize <- responseBodySize.fold(none[Long].pure[F])(
          _.freeze(response.flatMap(_.knownBodySize))
        )
        terminal = startAttributes ++ response.fold(Attributes.empty)(_.attributes) ++
          error.map(TypedClientAttributes.errorType)
        _ <- runInContext {
          val recordDuration = duration.record(elapsed.toUnit(TimeUnit.SECONDS), terminal)
          // Attempt each enabled histogram independently, then balance active requests.
          val recordRequest = requestBodySize.fold(recordDuration)(recording =>
            recordDuration.guarantee(recording.record(requestSize, terminal))
          )
          val recordResponse = responseBodySize.fold(recordRequest)(recording =>
            recordRequest.guarantee(recording.record(responseSize, terminal))
          )
          recordResponse.guarantee(active.traverse_(_.dec(startAttributes)))
        }
      } yield ()

    def observeRequest(request: Request[F]): Request[F] =
      requestBodySize.flatMap(_.observe(request.body)).fold(request)(request.withBodyStream)

    def observeResponse(request: RequestPrelude, response: Response[F]): F[Response[F]] = {
      val (knownSize, observed) = responseBodySize.fold((none[Long], response)) { recording =>
        val noBody = request.method == Method.HEAD || !response.status.isEntityAllowed ||
          response.status.responseClass == Status.Informational ||
          (request.method == Method.CONNECT && response.status.responseClass == Status.Successful)
        if (noBody) (Some(0L), response)
        else
          (
            recording.contentLength(response.headers),
            recording.observe(response.body).fold(response)(response.withBodyStream),
          )
      }
      responseData
        .set(
          Some(
            ResponseData(completionAttributes(request, response.responsePrelude, config), knownSize)
          )
        )
        .as(observed)
    }
  }
}
