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
package otel4s.middleware.trace
package client

import org.http4s.otel4s.middleware.client.UriTemplateClassifier
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

/** Utility for creating built-in [[`SpanDataProvider`]]s for a client. */
object ClientSpanDataProvider {

  /** Opt-In `Attribute` configuration. */
  private[this] final case class OptIn(
      httpRequestHeaders: Option[HeaderRedactor] = None,
      httpResponseHeaders: Option[HeaderRedactor] = None,
      urlScheme: Boolean = false,
      urlTemplate: Boolean = false,
      userAgentOriginal: Boolean = false,
  )

  /** Data shared by OpenTelemetry span name and request `Attributes`. */
  private[this] final case class OtelData(
      httpRequestMethod: Attribute[String],
      urlTemplate: Option[Attribute[String]],
  ) {
    val requestMethodIsUnknown: Boolean =
      httpRequestMethod == TypedClientTraceAttributes.httpRequestMethodOther
  }

  /** An immutable builder for configuring a [[`SpanDataProvider`]] consistent
    * with OpenTelemetry semantic conventions.
    */
  sealed trait OpenTelemetryBuilder { self: SpanDataProvider =>

    /** Sets how to determine the template of an absolute path reference from a
      * URL. The classifier is used for span names and for the `url.template`
      * `Attribute` (when opted into).
      */
    def withUrlTemplateClassifier(
        urlTemplateClassifier: UriTemplateClassifier
    ): SpanDataProvider with OpenTelemetryBuilder

    /** Opts into providing the `http.request.header.<key>` `Attribute`s for
      * spans, the headers for which will be redacted by the given `redactor`.
      */
    def optIntoHttpRequestHeaders(
        redactor: HeaderRedactor
    ): SpanDataProvider with OpenTelemetryBuilder

    /** Opts into providing the `http.response.header.<key>` `Attribute`s for
      * spans, the headers for which will be redacted by the given `redactor`.
      */
    def optIntoHttpResponseHeaders(
        redactor: HeaderRedactor
    ): SpanDataProvider with OpenTelemetryBuilder

    /** Opts into providing the `url.scheme` `Attribute` for spans. */
    def optIntoUrlScheme: SpanDataProvider with OpenTelemetryBuilder

    /** Opts into providing the `url.template` `Attribute` for spans. */
    def optIntoUrlTemplate: SpanDataProvider with OpenTelemetryBuilder

    /** Opts into providing the `user_agent.original` `Attribute` for spans. */
    def optIntoUserAgentOriginal: SpanDataProvider with OpenTelemetryBuilder
  }

  // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client
  private[this] final class OtelProvider(
      urlRedactor: UriRedactor,
      urlTemplateClassifier: UriTemplateClassifier,
      optIn: OptIn,
  ) extends SpanDataProvider
      with OpenTelemetryBuilder {
    type Shared = OtelData

    def processSharedData[F[_]](request: Request[F]): OtelData =
      OtelData(
        TypedClientTraceAttributes.httpRequestMethod(request.method),
        TypedClientTraceAttributes.Experimental
          .urlTemplate(request.uri, urlTemplateClassifier),
      )

    def spanName[F[_]](request: Request[F], sharedProcessedData: OtelData): String = {
      val method =
        if (sharedProcessedData.requestMethodIsUnknown) "HTTP"
        else sharedProcessedData.httpRequestMethod.value
      sharedProcessedData.urlTemplate
        .fold(method)(attr => s"$method ${attr.value}")
    }

    def requestAttributes[F[_]](request: Request[F], sharedProcessedData: OtelData): Attributes = {
      val b = Attributes.newBuilder

      b += sharedProcessedData.httpRequestMethod
      b ++= TypedClientTraceAttributes.serverAddress(request.uri.host)
      b ++= TypedClientTraceAttributes.serverPort(request.remotePort, request.uri)
      b += TypedClientTraceAttributes.urlFull(request.uri, urlRedactor)
      // `error.type` handled by `responseAttributes` and `exceptionAttributes`
      if (sharedProcessedData.requestMethodIsUnknown) {
        b += TypedClientTraceAttributes.httpRequestMethodOriginal(request.method)
      }
      // `http.response.status_code` handled by `responseAttributes`
      // `network.protocol.name` not required because http4s only supports http
      //   and `Request#httpVersion` is always populated/available
      b ++= TypedClientTraceAttributes.httpRequestResendCount(request)
      request.remote.foreach { socketAddress =>
        b += TypedClientTraceAttributes.networkPeerAddress(socketAddress.host)
        b += TypedClientTraceAttributes.networkPeerPort(socketAddress.port)
      }
      b += TypedClientTraceAttributes.networkProtocolVersion(request.httpVersion)
      // `http.request.body.size` not available to opt into at this time
      optIn.httpRequestHeaders.foreach { redactor =>
        TypedClientTraceAttributes
          .httpRequestHeadersForBuilder(request.headers, redactor)(b)
      }
      // `http.request.size` not available to opt into at this time
      // `http.response.body.size` not available to opt into at this time
      // `http.response.header.<key>`s handled by `responseAttributes`
      // `http.response.size` not available to opt into at this time
      // `network.transport` not available to opt into at this time
      if (optIn.urlScheme) {
        b ++= TypedClientTraceAttributes.urlScheme(request.uri.scheme)
      }
      if (optIn.urlTemplate) {
        b ++= sharedProcessedData.urlTemplate
      }
      if (optIn.userAgentOriginal) {
        b ++= TypedClientTraceAttributes.userAgentOriginal(request.headers)
      }
      // `user_agent.synthetic.type` not available to opt into at this time

      b.result()
    }

    def responseAttributes[F[_]](response: Response[F]): Attributes = {
      val b = Attributes.newBuilder

      if (!response.status.isSuccess) {
        // `error.type` for a `Throwable` handled by `exceptionAttributes`
        b += TypedClientTraceAttributes.errorType(response.status)
      }
      b += TypedClientTraceAttributes.httpResponseStatusCode(response.status)
      optIn.httpResponseHeaders.foreach { redactor =>
        TypedClientTraceAttributes
          .httpResponseHeadersForBuilder(response.headers, redactor)(b)
      }

      b.result()
    }

    def exceptionAttributes(cause: Throwable): Attributes =
      Attributes(TypedClientTraceAttributes.errorType(cause))

    private def copy(
        urlTemplateClassifier: UriTemplateClassifier = this.urlTemplateClassifier,
        optIn: OptIn = this.optIn,
    ): OtelProvider =
      new OtelProvider(
        this.urlRedactor,
        urlTemplateClassifier,
        optIn,
      )
    def withUrlTemplateClassifier(
        urlTemplateClassifier: UriTemplateClassifier
    ): OtelProvider =
      copy(urlTemplateClassifier = urlTemplateClassifier)
    def optIntoHttpRequestHeaders(redactor: HeaderRedactor): OtelProvider =
      copy(optIn = optIn.copy(httpRequestHeaders = Some(redactor)))
    def optIntoHttpResponseHeaders(redactor: HeaderRedactor): OtelProvider =
      copy(optIn = optIn.copy(httpResponseHeaders = Some(redactor)))
    def optIntoUrlScheme: OtelProvider =
      copy(optIn = optIn.copy(urlScheme = true))
    def optIntoUrlTemplate: OtelProvider =
      copy(optIn = optIn.copy(urlTemplate = true))
    def optIntoUserAgentOriginal: OtelProvider =
      copy(optIn = optIn.copy(userAgentOriginal = true))
  }

  /** @param urlRedactor a redactor with which to sanitize requests' URLs
    *                    before using them as values for `Attribute`s.
    * @return a configurable [[`SpanDataProvider`]] following OpenTelemetry
    *         semantic conventions
    */
  def openTelemetry(urlRedactor: UriRedactor): SpanDataProvider with OpenTelemetryBuilder =
    new OtelProvider(urlRedactor, UriTemplateClassifier.indeterminate, OptIn())
}
