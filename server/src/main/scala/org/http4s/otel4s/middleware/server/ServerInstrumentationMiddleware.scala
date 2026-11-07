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

package org.http4s.otel4s.middleware.server

import cats.effect.Outcome
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.syntax.all._
import cats.syntax.all._
import cats.~>
import fs2.Stream
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.otel4s.middleware.ExchangeCompletion
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsRecorder
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Span
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

private[server] final class ServerInstrumentationMiddleware[
    F[_]: Temporal: Tracer: ContextManager
] private (
    recorder: ServerMetricsRecorder[F],
    metricsConfig: ServerMetricsConfig,
    tracingConfig: ServerTracingConfig,
) {
  import ServerInstrumentationMiddleware._

  def apply(app: HttpApp[F]): HttpApp[F] = HttpApp[F] { request =>
    (recorder.isEnabled, Tracer[F].meta.isEnabled).tupled.flatMap {
      case (false, false) => app(request)
      case (metricsEnabled, tracingEnabled) =>
        val prelude = request.requestPrelude
        val metered = metricsEnabled && metricsConfig.requestFilter(prelude).isEnabled
        val traced = tracingEnabled && tracingConfig.requestFilter(prelude).isEnabled
        if (!metered && !traced) app(request)
        else {
          val exchange = instrument(app, request, prelude, metered, traced)
          if (traced) Tracer[F].joinOrRoot(request.headers)(exchange) else exchange
        }
    }
  }

  private def instrument(
      app: HttpApp[F],
      request: Request[F],
      prelude: RequestPrelude,
      metered: Boolean,
      traced: Boolean,
  ): F[Response[F]] = Temporal[F].uncancelable { poll =>
    for {
      exchange <- startExchange(request, prelude, metered, traced)
      response <- poll(
        exchange.runInContext(
          Temporal[F].unit
            .flatMap(_ => app(exchange.observeRequest(request)))
            .flatMap(propagateResponse(_, prelude, traced))
        )
      ).flatMap(exchange.observeResponse).guaranteeCase {
        case Outcome.Errored(error) => exchange.finish(Some(error))
        case Outcome.Canceled() => exchange.finish(None)
        case Outcome.Succeeded(_) => Temporal[F].unit
      }
    } yield response
  }

  private def propagateResponse(
      response: Response[F],
      request: RequestPrelude,
      traced: Boolean,
  ): F[Response[F]] =
    if (traced && tracingConfig.reversePropagationFilter(request).isEnabled)
      Tracer[F].propagate(Headers.empty).map { headers =>
        if (headers.headers.isEmpty) response
        else response.withHeaders(response.headers ++ headers)
      }
    else response.pure[F]

  private def startSpan(request: Request[F], start: ExchangeCompletion.Started): F[Span[F]] = {
    val provider = tracingConfig.spanDataProvider
    val noBody = request.withBodyStream(Stream.empty)
    val shared = provider.processSharedData(noBody)
    Tracer[F]
      .spanBuilder(provider.spanName(noBody, shared))
      .withSpanKind(SpanKind.Server)
      .addAttributes(provider.requestAttributes(noBody, shared))
      .withStartTimestamp(start.timestamp)
      .build
      .startUnmanaged
  }

  private def startExchange(
      request: Request[F],
      prelude: RequestPrelude,
      metered: Boolean,
      traced: Boolean,
  ): F[Exchange[F]] = for {
    start <- ExchangeCompletion.started[F]
    span <- if (traced) startSpan(request, start).map(_.some) else none[Span[F]].pure[F]
    runInContext <- span.fold(ContextManager[F].captureScope) { span =>
      Tracer[F].childScope(span.context)(ContextManager[F].captureScope)
    }
    response <- Ref.of[F, Option[Response[F]]](None)
    tracing = span.map(new TraceRecording(_, response, prelude, tracingConfig))
    recording <- Temporal[F].unit
      .flatMap { _ =>
        if (metered) runInContext(recorder.start(request, prelude)).map(_.some)
        else none[ServerMetricsRecorder.Exchange[F]].pure[F]
      }
      .onError { case _ =>
        Temporal[F].realTime.flatMap(timestamp => runInContext(span.traverse_(_.end(timestamp))))
      }
    completion <- ExchangeCompletion.create[F](
      start,
      terminal =>
        response.get.flatMap { result =>
          recording.traverse_(
            _.record(terminal.elapsed, result.map(_.responsePrelude), terminal.error)
          )
        },
      terminal => runInContext(tracing.traverse_(_.finish(terminal))),
    )
  } yield new Exchange(completion, response, runInContext, recording)
}

private[server] object ServerInstrumentationMiddleware {
  def create[F[_]: Temporal: MeterProvider: Tracer: ContextManager](
      metricsConfig: ServerMetricsConfig,
      tracingConfig: ServerTracingConfig,
  ): F[HttpApp[F] => HttpApp[F]] =
    ServerMetricsRecorder
      .create[F](metricsConfig)
      .map { recorder =>
        new ServerInstrumentationMiddleware(
          recorder,
          metricsConfig,
          tracingConfig,
        ).apply
      }

  /** Records response attributes and ends the span, including when a callback fails. */
  private final class TraceRecording[F[_]: Temporal](
      span: Span[F],
      response: Ref[F, Option[Response[F]]],
      request: RequestPrelude,
      config: ServerTracingConfig,
  ) {
    def finish(terminal: ExchangeCompletion.Terminal): F[Unit] = {
      val provider = config.spanDataProvider
      (for {
        result <- response.get
        statusError = result.exists(r => config.errorClassifier.isError(request, r.responsePrelude))
        attributes = result.fold(Attributes.empty) { response =>
          provider.responseAttributes(response) ++
            (if (statusError) provider.errorAttributes(request, response.responsePrelude)
             else Attributes.empty)
        } ++ terminal.error.fold(Attributes.empty)(provider.exceptionAttributes)
        _ <- span.addAttributes(attributes)
        _ <- span.setStatus(StatusCode.Error).whenA(statusError || terminal.error.nonEmpty)
        _ <- terminal.error.traverse_(cause => span.recordException(cause))
      } yield ()).guarantee(span.end(terminal.timestamp))
    }
  }

  private final class Exchange[F[_]: Temporal](
      completion: ExchangeCompletion[F],
      response: Ref[F, Option[Response[F]]],
      val runInContext: F ~> F,
      recording: Option[ServerMetricsRecorder.Exchange[F]],
  ) {
    def finish(error: Option[Throwable]): F[Unit] = completion.finish(error)

    def observeRequest(request: Request[F]): Request[F] =
      recording.fold(request)(_.observeRequest(request))

    def observeResponse(value: Response[F]): F[Response[F]] =
      response.set(Some(value.withBodyStream(Stream.empty))).as {
        val body = recording.fold(value.body)(_.observeResponse(value))
        value.withBodyStream(observeBody(body))
      }

    private def observeBody(body: Stream[F, Byte]): Stream[F, Byte] =
      Stream.eval(Ref.of[F, Option[Throwable]](None)).flatMap { error =>
        body
          .translate(runInContext)
          .scope
          .handleErrorWith { cause =>
            Stream.exec(error.set(Some(cause))) ++ Stream.raiseError[F](cause)
          }
          .onFinalize(error.get.flatMap(completion.finish))
      }
  }
}
