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
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.ResponsePrelude
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
) {
  // Instrument metadata may change, so check it for each acquisition.
  def isEnabled: F[Boolean] =
    duration.backend.meta.isEnabled.ifM(
      true.pure[F],
      active.fold(false.pure[F])(_.backend.meta.isEnabled),
    )

  def start(request: Request[F], prelude: RequestPrelude): F[ClientMetricsRecorder.Exchange[F]] = {
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
      responseAttributes <- Ref.of[F, Option[Attributes]](None)
      runInContext = new (F ~> F) {
        def apply[A](fa: F[A]): F[A] = otel4s.localContext.scope(fa)(context)
      }
      exchange = new ClientMetricsRecorder.Exchange(
        responseAttributes,
        startAttributes,
        runInContext,
        duration,
        active,
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
    } yield new ClientMetricsRecorder(otel4s, config, duration, active)

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

  /** Holds metric attributes and recording context, without owning completion.
    * The middleware's Lifecycle calls `record` once in its uncancelable completion.
    */
  final class Exchange[F[_]: Temporal] private[ClientMetricsRecorder] (
      responseAttributes: Ref[F, Option[Attributes]],
      startAttributes: Attributes,
      runInContext: F ~> F,
      duration: Histogram[F, Double],
      active: Option[UpDownCounter[F, Long]],
      config: ClientMetricsConfig,
  ) {
    // Once response attributes are stored, the body or response-release finalizer owns completion.
    // Until then, the outer acquisition finalizer records acquisition failure or cancellation.
    def hasResponse: F[Boolean] = responseAttributes.get.map(_.nonEmpty)

    def record(elapsed: FiniteDuration, error: Option[Throwable]): F[Unit] =
      responseAttributes.get.flatMap { response =>
        val terminal = startAttributes ++
          response.getOrElse(Attributes.empty) ++
          error.map(TypedClientAttributes.errorType)

        runInContext(
          duration
            .record(elapsed.toUnit(TimeUnit.SECONDS), terminal)
            .guarantee(active.traverse_(_.dec(startAttributes)))
        )
      }

    def observeResponse(request: RequestPrelude, response: ResponsePrelude): F[Unit] =
      responseAttributes.set(Some(completionAttributes(request, response, config)))
  }
}
