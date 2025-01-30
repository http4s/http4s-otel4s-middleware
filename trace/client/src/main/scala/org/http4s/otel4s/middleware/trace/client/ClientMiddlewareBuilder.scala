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
package trace
package client

import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.Outcome
import cats.effect.Resource
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.Stream
import org.http4s.client.Client
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.TracerProvider

/** Middleware builder for wrapping an http4s `Client` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client]]
  */
class ClientMiddlewareBuilder[F[_]: TracerProvider: Concurrent] private (
    urlRedactor: UriRedactor,
    spanDataProvider: SpanDataProvider,
    urlTemplateClassifier: UriTemplateClassifier,
    headersAllowedAsAttributes: HeadersAllowedAsAttributes,
    perRequestTracingFilter: PerRequestTracingFilter,
) {
  private[this] def copy(
      spanDataProvider: SpanDataProvider = this.spanDataProvider,
      urlTemplateClassifier: UriTemplateClassifier = this.urlTemplateClassifier,
      headersAllowedAsAttributes: HeadersAllowedAsAttributes = this.headersAllowedAsAttributes,
      perRequestTracingFilter: PerRequestTracingFilter = this.perRequestTracingFilter,
  ): ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](
      this.urlRedactor,
      spanDataProvider,
      urlTemplateClassifier,
      headersAllowedAsAttributes,
      perRequestTracingFilter,
    )

  /** Sets how a span's name and `Attributes` are derived from a request and
    * response.
    */
  def withSpanDataProvider(spanDataProvider: SpanDataProvider): ClientMiddlewareBuilder[F] =
    copy(spanDataProvider = spanDataProvider)

  /** Sets how to determine the template of an absolute path reference from a
    * URL.
    */
  def withUrlTemplateClassifier(
      urlTemplateClassifier: UriTemplateClassifier
  ): ClientMiddlewareBuilder[F] =
    copy(urlTemplateClassifier = urlTemplateClassifier)

  /** Sets which headers are allowed to be made into `Attribute`s. */
  def withHeadersAllowedAsAttributes(
      headersAllowedAsAttributes: HeadersAllowedAsAttributes
  ): ClientMiddlewareBuilder[F] =
    copy(headersAllowedAsAttributes = headersAllowedAsAttributes)

  /** Sets a filter that determines whether each request and its response
    * should be traced.
    */
  def withPerRequestTracingFilter(
      perRequestTracingFilter: PerRequestTracingFilter
  ): ClientMiddlewareBuilder[F] =
    copy(perRequestTracingFilter = perRequestTracingFilter)

  /** @return the configured middleware */
  def build: F[Client[F] => Client[F]] =
    for {
      tracer <- TracerProvider[F]
        .tracer("org.http4s.otel4s.middleware.client")
        .withVersion(org.http4s.otel4s.middleware.BuildInfo.version)
        .withSchemaUrl("https://opentelemetry.io/schemas/1.29.0")
        .get
    } yield (client: Client[F]) =>
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]
        if (
          !perRequestTracingFilter(req.requestPrelude).isEnabled ||
          !tracer.meta.isEnabled
        ) {
          client.run(req)
        } else {
          val reqNoBody = req.withBodyStream(Stream.empty)
          val shared =
            spanDataProvider.processSharedData(
              reqNoBody,
              urlTemplateClassifier,
              urlRedactor,
            )
          val spanName =
            spanDataProvider.spanName(
              reqNoBody,
              urlTemplateClassifier,
              urlRedactor,
              shared,
            )
          val reqAttributes =
            spanDataProvider.requestAttributes(
              reqNoBody,
              urlTemplateClassifier,
              urlRedactor,
              shared,
              headersAllowedAsAttributes.request,
            )

          MonadCancelThrow[Resource[F, *]].uncancelable { poll =>
            for {
              res <- tracer
                .spanBuilder(spanName)
                .withSpanKind(SpanKind.Client)
                .addAttributes(reqAttributes)
                .build
                .resource
              span = res.span
              trace = res.trace
              traceHeaders <- Resource.eval(tracer.propagate(Headers.empty)).mapK(trace)
              newReq = req.withHeaders(traceHeaders ++ req.headers)

              resp <- poll(client.run(newReq).mapK(trace)).guaranteeCase {
                case Outcome.Succeeded(fa) =>
                  fa.evalMap { resp =>
                    val respAttributes =
                      spanDataProvider.responseAttributes(
                        resp.withBodyStream(Stream.empty),
                        headersAllowedAsAttributes.response,
                      )
                    span.addAttributes(respAttributes) >> span
                      .setStatus(StatusCode.Error)
                      .unlessA(resp.status.isSuccess)
                  }

                case Outcome.Errored(e) =>
                  Resource.eval {
                    span.addAttributes(spanDataProvider.exceptionAttributes(e))
                  }

                case Outcome.Canceled() =>
                  Resource.unit
              }
            } yield resp
          }
        }
      }
}

object ClientMiddlewareBuilder {

  /** @return a client middleware builder with default configuration */
  def default[F[_]: TracerProvider: Concurrent](
      urlRedactor: UriRedactor
  ): ClientMiddlewareBuilder[F] =
    new ClientMiddlewareBuilder[F](
      urlRedactor,
      Defaults.spanDataProvider,
      Defaults.urlTemplateClassifier,
      Defaults.headersAllowedAsAttributes,
      Defaults.perRequestTracingFilter,
    )

  /** The default configuration values for a client middleware builder. */
  object Defaults {
    def spanDataProvider: SpanDataProvider = SpanDataProvider.default
    def urlTemplateClassifier: UriTemplateClassifier =
      UriTemplateClassifier.indeterminate
    def headersAllowedAsAttributes: HeadersAllowedAsAttributes =
      HeadersAllowedAsAttributes.default
    def perRequestTracingFilter: PerRequestTracingFilter =
      PerRequestTracingFilter.default
  }
}
