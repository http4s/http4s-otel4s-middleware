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

private[client] object ClientInstrumentationMiddleware {

  def create[F[_]: Temporal](
      otel4s: Otel4s[F],
      tracer: Tracer[F],
      metricsConfig: ClientMetricsConfig,
      tracingConfig: ClientTracingConfig,
  ): F[Client[F] => Client[F]] =
    ClientMetricsRecorder.create(otel4s, metricsConfig).map { recorder =>
      val provider = tracingConfig.spanDataProvider

      def startExchange(
          request: Request[F],
          prelude: RequestPrelude,
          metered: Boolean,
          traced: Boolean,
      ): F[Exchange[F]] = for {
        start <- ExchangeCompletion.started[F]
        span <-
          if (traced) {
            val noBody = request.withBodyStream(Stream.empty)
            val shared = provider.processSharedData(noBody)
            tracer
              .spanBuilder(provider.spanName(noBody, shared))
              .withSpanKind(SpanKind.Client)
              .addAttributes(provider.requestAttributes(noBody, shared))
              .withStartTimestamp(start.timestamp)
              .build
              .startUnmanaged
              .map(_.some)
          } else none[Span[F]].pure[F]
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

      base =>
        Client[F] { request =>
          Resource.eval((recorder.isEnabled, tracer.meta.isEnabled).tupled).flatMap {
            case (false, false) => base.run(request)
            case (metricsEnabled, tracingEnabled) =>
              val prelude = request.requestPrelude
              val metered = metricsEnabled && metricsConfig.requestFilter(prelude).isEnabled
              val traced = tracingEnabled && tracingConfig.requestFilter(prelude).isEnabled
              if (!metered && !traced) base.run(request)
              else
                Resource
                  .makeCase(startExchange(request, prelude, metered, traced)) { (exchange, exit) =>
                    exchange.hasResponse.ifM(
                      Temporal[F].unit,
                      exchange.completion.finish(exit match {
                        case Resource.ExitCase.Errored(cause) => Some(cause)
                        case _ => None
                      }),
                    )
                  }
                  .flatMap { exchange =>
                    Resource
                      .eval(
                        exchange.runInContext(
                          if (traced && tracingConfig.propagationFilter(prelude).isEnabled)
                            tracer.propagate(Headers.empty)
                          else Temporal[F].pure(Headers.empty)
                        )
                      )
                      .flatMap { headers =>
                        val outgoing =
                          if (headers.headers.isEmpty) request
                          else request.withHeaders(request.headers ++ headers)
                        base
                          .run(exchange.observeRequest(outgoing))
                          .mapK(exchange.runInContext)
                          .flatMap { response =>
                            Resource.make(
                              exchange
                                .observeResponse(prelude, response)
                                .map(response =>
                                  response.withBodyStream(exchange.observe(response.body))
                                )
                            )(_ => exchange.completion.finish(None))
                          }
                      }
                  }
          }
        }
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
      val completion: ExchangeCompletion[F],
      val runInContext: F ~> F,
      tracing: Option[TraceRecording[F]],
      recording: Option[ClientMetricsRecorder.Exchange[F]],
  ) {
    // Response release handles completion once metrics response classification succeeds.
    // When metrics are disabled, check whether tracing has stored a response instead.
    def hasResponse: F[Boolean] =
      recording.fold(tracing.fold(false.pure[F])(_.hasResponse))(_.hasResponse)

    def observeRequest(request: Request[F]): Request[F] =
      recording.fold(request)(_.observeRequest(request))

    def observeResponse(request: RequestPrelude, response: Response[F]): F[Response[F]] =
      tracing.traverse_(_.observeResponse(response)).flatMap { _ =>
        recording.fold(Temporal[F].pure(response))(_.observeResponse(request, response))
      }

    def observe(body: Stream[F, Byte]): Stream[F, Byte] = {
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
