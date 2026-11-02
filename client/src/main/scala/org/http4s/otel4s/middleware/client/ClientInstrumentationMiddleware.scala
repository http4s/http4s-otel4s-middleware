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

package org.http4s.otel4s.middleware.client

import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.syntax.all._
import cats.syntax.all._
import cats.~>
import fs2.Stream
import org.http4s.Headers
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.otel4s.middleware.ExchangeCompletion
import org.http4s.otel4s.middleware.instances.textMap._
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsRecorder
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.trace.Span
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

private[client] final class ClientInstrumentationMiddleware[F[_]: Temporal] private (
    otel4s: Otel4s[F],
    tracer: Tracer[F],
    recorder: ClientMetricsRecorder[F],
    metricsConfig: ClientMetricsConfig,
    tracingConfig: ClientTracingConfig,
) {
  import ClientInstrumentationMiddleware._

  def apply(base: Client[F]): Client[F] =
    Client[F] { request =>
      Resource.eval((recorder.isEnabled, tracer.meta.isEnabled).tupled).flatMap {
        case (false, false) =>
          base.run(request)

        case (metricsEnabled, tracingEnabled) =>
          val prelude = request.requestPrelude
          val metered = metricsEnabled && metricsConfig.requestFilter(prelude).isEnabled
          val traced = tracingEnabled && tracingConfig.requestFilter(prelude).isEnabled
          if (!metered && !traced) base.run(request)
          else instrument(base, request, prelude, metered, traced)
      }
    }

  private def instrument(
      base: Client[F],
      request: Request[F],
      prelude: RequestPrelude,
      metered: Boolean,
      traced: Boolean,
  ): Resource[F, Response[F]] =
    for {
      exchange <- Resource.makeCase(startExchange(request, prelude, metered, traced)) {
        (exchange, exit) => exchange.finishAcquisition(exit)
      }
      outgoing <- Resource.eval(
        exchange.runInContext(propagateRequest(request, prelude, traced))
      )
      response <- base.run(exchange.observeRequest(outgoing)).mapK(exchange.runInContext)
      observed <- exchange.observeResponse(prelude, response)
    } yield observed

  private def propagateRequest(
      request: Request[F],
      prelude: RequestPrelude,
      traced: Boolean,
  ): F[Request[F]] =
    if (traced && tracingConfig.propagationFilter(prelude).isEnabled)
      tracer.propagate(Headers.empty).map { headers =>
        if (headers.headers.isEmpty) request
        else request.withHeaders(request.headers ++ headers)
      }
    else request.pure[F]

  private def startSpan(
      request: Request[F],
      start: ExchangeCompletion.Started,
  ): F[Span[F]] = {
    val provider = tracingConfig.spanDataProvider
    val noBody = request.withBodyStream(Stream.empty)
    val shared = provider.processSharedData(noBody)
    tracer
      .spanBuilder(provider.spanName(noBody, shared))
      .withSpanKind(SpanKind.Client)
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
  ): F[Exchange[F]] =
    for {
      start <- ExchangeCompletion.started[F]
      span <- if (traced) startSpan(request, start).map(_.some) else none[Span[F]].pure[F]
      context <- span.fold(otel4s.localContext.ask[otel4s.Ctx]) { span =>
        tracer.childScope(span.context)(otel4s.localContext.ask[otel4s.Ctx])
      }
      runInContext = new (F ~> F) {
        def apply[A](fa: F[A]): F[A] = otel4s.localContext.scope(fa)(context)
      }
      tracing <- span.traverse { span =>
        Ref
          .of[F, Option[Response[F]]](None)
          .map(response => new TraceRecording(span, response, prelude, tracingConfig))
      }
      recording <- Temporal[F].unit
        .flatMap(_ =>
          if (metered) runInContext(recorder.start(request, prelude)).map(_.some)
          else none[ClientMetricsRecorder.Exchange[F]].pure[F]
        )
        .onError { case _ =>
          Temporal[F].realTime
            .flatMap(timestamp => runInContext(span.traverse_(_.end(timestamp))))
        }
      completion <- ExchangeCompletion.create[F](
        start,
        terminal => recording.traverse_(_.record(terminal.elapsed, terminal.error)),
        terminal => runInContext(tracing.traverse_(_.finish(terminal))),
      )
    } yield new Exchange(completion, runInContext, tracing, recording)
}

private[client] object ClientInstrumentationMiddleware {

  def create[F[_]: Temporal](
      otel4s: Otel4s[F],
      tracer: Tracer[F],
      metricsConfig: ClientMetricsConfig,
      tracingConfig: ClientTracingConfig,
  ): F[Client[F] => Client[F]] =
    ClientMetricsRecorder.create(otel4s, metricsConfig).map { recorder =>
      new ClientInstrumentationMiddleware(
        otel4s,
        tracer,
        recorder,
        metricsConfig,
        tracingConfig,
      ).apply
    }

  /** Stores the response without its body. Ends the span even if recording attributes fails. */
  private final class TraceRecording[F[_]: Temporal](
      span: Span[F],
      response: Ref[F, Option[Response[F]]],
      request: RequestPrelude,
      config: ClientTracingConfig,
  ) {
    def hasResponse: F[Boolean] = response.get.map(_.nonEmpty)

    def observeResponse(value: Response[F]): F[Unit] =
      response.set(Some(value.withBodyStream(Stream.empty)))

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
      val runInContext: F ~> F,
      tracing: Option[TraceRecording[F]],
      recording: Option[ClientMetricsRecorder.Exchange[F]],
  ) {
    // Response release handles completion once metrics response classification succeeds.
    // When metrics are disabled, check whether tracing has stored a response instead.
    private def hasResponse: F[Boolean] =
      recording.fold(tracing.fold(false.pure[F])(_.hasResponse))(_.hasResponse)

    def observeRequest(request: Request[F]): Request[F] =
      recording.fold(request)(_.observeRequest(request))

    def finishAcquisition(exit: Resource.ExitCase): F[Unit] =
      hasResponse.ifM(
        Temporal[F].unit,
        completion.finish(exit match {
          case Resource.ExitCase.Errored(cause) => Some(cause)
          case _ => None
        }),
      )

    def observeResponse(
        request: RequestPrelude,
        response: Response[F],
    ): Resource[F, Response[F]] =
      Resource.make(
        tracing.traverse_(_.observeResponse(response)).flatMap { _ =>
          recording
            .fold(response.pure[F])(_.observeResponse(request, response))
            .map(response => response.withBodyStream(observeBody(response.body)))
        }
      )(_ => completion.finish(None))

    private def observeBody(body: Stream[F, Byte]): Stream[F, Byte] = {
      val observed = Stream.eval(Ref.of[F, Option[Throwable]](None)).flatMap { error =>
        body.scope
          .handleErrorWith { cause =>
            Stream.exec(error.set(Some(cause))) ++ Stream.raiseError[F](cause)
          }
          .onFinalize(error.get.flatMap(completion.finish))
      }
      if (tracing.nonEmpty) observed.translate(runInContext) else observed
    }
  }
}
