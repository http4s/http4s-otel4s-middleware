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

import cats.effect.Temporal
import cats.effect.syntax.all._
import cats.syntax.all._
import cats.~>
import fs2.Stream
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.ResponsePrelude
import org.http4s.Uri
import org.http4s.headers.Forwarded
import org.http4s.otel4s.middleware.BuildInfo
import org.http4s.otel4s.middleware.metrics.BodySizeRecording
import org.http4s.otel4s.middleware.server.OriginalScheme
import org.http4s.otel4s.middleware.server.TypedServerAttributes
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.metrics.Histogram
import org.typelevel.otel4s.metrics.UpDownCounter
import org.typelevel.otel4s.semconv.SchemaUrls
import org.typelevel.otel4s.semconv.attributes.ServerAttributes

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** Instruments and per-exchange recording, independent of completion ownership. */
private[middleware] final class ServerMetricsRecorder[F[_]: Temporal] private[server] (
    otel4s: Otel4s[F],
    config: ServerMetricsConfig,
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

  // Instrument metadata may change, so check it for each invocation.
  def isEnabled: F[Boolean] = duration.backend.meta.isEnabled.ifM(
    true.pure[F],
    optionalInstrumentMetadata.existsM(_.isEnabled),
  )

  def start(request: Request[F], prelude: RequestPrelude): F[ServerMetricsRecorder.Exchange[F]] = {
    val labels = ServerMetricsRecorder.requestLabels(request, prelude, config)
    val terminal = {
      val builder = Attributes.newBuilder
      builder ++= labels
      builder += TypedServerAttributes.networkProtocolVersion(prelude.httpVersion)
      builder ++= TypedServerAttributes.httpRoute(prelude, config.routeClassifier)
      builder.result()
    }
    for {
      context <- otel4s.localContext.ask[otel4s.Ctx]
      requestSize <- BodySizeRecording.create(
        config.requestBodySize,
        requestBodySize,
        prelude.headers,
      )
      responseSize <- BodySizeRecording.create(config.responseBodySize, responseBodySize)
      runInContext = new (F ~> F) {
        def apply[A](fa: F[A]): F[A] = otel4s.localContext.scope(fa)(context)
      }
      exchange = new ServerMetricsRecorder.Exchange(
        labels,
        terminal,
        prelude,
        config,
        runInContext,
        duration,
        active,
        requestSize,
        responseSize,
      )
      _ <- active.traverse_(_.inc(labels))
    } yield exchange
  }
}

private[middleware] object ServerMetricsRecorder {

  def create[F[_]: Temporal](
      otel4s: Otel4s[F],
      config: ServerMetricsConfig,
  ): F[ServerMetricsRecorder[F]] =
    for {
      meter <- otel4s.meterProvider
        .meter("org.http4s.otel4s.middleware.server")
        .withVersion(BuildInfo.version)
        .withSchemaUrl(SchemaUrls.V1_43_0)
        .get
      duration <- meter
        .histogram[Double]("http.server.request.duration")
        .withUnit("s")
        .withDescription("Duration of HTTP server requests.")
        .withExplicitBucketBoundaries(config.responseDurationSecondsHistogramBuckets)
        .create
      active <-
        if (config.activeRequestsEnabled)
          meter
            .upDownCounter[Long]("http.server.active_requests")
            .withUnit("{request}")
            .withDescription("Number of active HTTP server requests.")
            .create
            .map(_.some)
        else none[UpDownCounter[F, Long]].pure[F]
      requestBodySize <- config.requestBodySize.traverse { sizeConfig =>
        meter
          .histogram[Long]("http.server.request.body.size")
          .withUnit("By")
          .withDescription("Size of HTTP server request bodies.")
          .withExplicitBucketBoundaries(sizeConfig.bucketBoundaries)
          .create
      }
      responseBodySize <- config.responseBodySize.traverse { sizeConfig =>
        meter
          .histogram[Long]("http.server.response.body.size")
          .withUnit("By")
          .withDescription("Size of HTTP server response bodies.")
          .withExplicitBucketBoundaries(sizeConfig.bucketBoundaries)
          .create
      }
    } yield new ServerMetricsRecorder(
      otel4s,
      config,
      duration,
      active,
      requestBodySize,
      responseBodySize,
    )

  private def requestLabels[F[_]](
      request: Request[F],
      prelude: RequestPrelude,
      config: ServerMetricsConfig,
  ): Attributes = {
    val forwarded = prelude.headers.get[Forwarded]
    val uri =
      if (prelude.uri.scheme.isDefined) prelude.uri
      else
        prelude.uri.copy(scheme = request.isSecure.map {
          case true => Uri.Scheme.https
          case false => Uri.Scheme.http
        })
    val scheme = OriginalScheme(forwarded, prelude.headers, uri)
    val server =
      if (config.serverAddressEnabled || config.serverPortEnabled)
        TypedServerAttributes.serverAddressAndPort(request, forwarded, scheme)
      else Attributes.empty
    val builder = Attributes.newBuilder
    builder ++= config.attributes
    builder ++= config.requestAttributes(prelude)
    builder += TypedServerAttributes.httpRequestMethod(prelude.method)
    builder ++= TypedServerAttributes.urlScheme(scheme)
    if (config.serverAddressEnabled) builder ++= server.get(ServerAttributes.ServerAddress)
    if (config.serverPortEnabled) builder ++= server.get(ServerAttributes.ServerPort)
    builder.result()
  }

  /** Holds attributes and full context. The lifecycle calls `record` once at completion. */
  final class Exchange[F[_]: Temporal] private[ServerMetricsRecorder] (
      startAttributes: Attributes,
      durationAttributes: Attributes,
      request: RequestPrelude,
      config: ServerMetricsConfig,
      val runInContext: F ~> F,
      duration: Histogram[F, Double],
      active: Option[UpDownCounter[F, Long]],
      requestBodySize: Option[BodySizeRecording[F]],
      responseBodySize: Option[BodySizeRecording[F]],
  ) {
    def record(
        elapsed: FiniteDuration,
        response: Option[ResponsePrelude],
        error: Option[Throwable],
    ): F[Unit] =
      runInContext(
        Temporal[F].unit
          .flatMap { _ =>
            val builder = Attributes.newBuilder
            builder ++= durationAttributes
            response.foreach { value =>
              builder += TypedServerAttributes.httpResponseStatusCode(value.status)
              if (config.errorClassifier.isError(request, value))
                builder += TypedServerAttributes.errorType(value.status)
            }
            builder ++= error.map(TypedServerAttributes.errorType)
            val terminal = builder.result()
            for {
              requestSize <- requestBodySize.fold(none[Long].pure[F])(_.freeze())
              responseSize <- responseBodySize.fold(none[Long].pure[F]) { recording =>
                val knownSize = response.flatMap { value =>
                  if (BodySizeRecording.noResponseBody(request.method, value.status)) Some(0L)
                  else recording.contentLength(value.headers)
                }
                recording.freeze(knownSize)
              }
              _ <- {
                val recordDuration = duration.record(elapsed.toUnit(TimeUnit.SECONDS), terminal)
                val recordRequest = requestBodySize.fold(recordDuration)(recording =>
                  recordDuration.guarantee(recording.record(requestSize, terminal))
                )
                responseBodySize.fold(recordRequest)(recording =>
                  recordRequest.guarantee(recording.record(responseSize, terminal))
                )
              }
            } yield ()
          }
          .guarantee(active.traverse_(_.dec(startAttributes)))
      )

    def observeRequest(request: Request[F]): Request[F] =
      requestBodySize.flatMap(_.observe(request.body)).fold(request)(request.withBodyStream)

    def observeResponse(response: Response[F]): Stream[F, Byte] =
      responseBodySize.fold(response.body) { recording =>
        if (BodySizeRecording.noResponseBody(request.method, response.status)) response.body
        else recording.observe(response.body).getOrElse(response.body)
      }

  }
}
