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
package trace.client

import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

/** Provides a name and attributes for spans using requests and responses.
  *
  * It is RECOMMENDED that callers pass `Request`s and `Response`s that have
  * had their bodies replaced with empty `Stream`s (by calling
  * `.withBodyStream(Stream.empty)`).
  *
  * @note Implementations MUST NOT access request or response bodies.
  */
trait SpanDataProvider extends AttributeProvider { self =>

  /** The type of shared processed data used to provide both the span name and
    * request `Attributes`.
    */
  type Shared

  /** Process data used to provide both the span name and request attributes.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def processSharedData[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
  ): Shared

  /** Provides the name for a span using the given request.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def spanName[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
      sharedProcessedData: Shared,
  ): String

  /** Provides attributes for a span using the given request.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def requestAttributes[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
      sharedProcessedData: Shared,
      headersAllowedAsAttributes: Set[CIString],
  ): Attributes

  final def requestAttributes[F[_]](
      request: Request[F],
      urlTemplateClassifier: UriTemplateClassifier,
      urlRedactor: UriRedactor,
      headersAllowedAsAttributes: Set[CIString],
  ): Attributes =
    requestAttributes(
      request,
      urlTemplateClassifier,
      urlRedactor,
      processSharedData(request, urlTemplateClassifier, urlRedactor),
      headersAllowedAsAttributes,
    )

  /** Returns an `AttributeProvider` that provides the attributes from this and
    * another `AttributeProvider`.
    *
    * If `that` is a `SpanAndAttributeProvider`, it will not be used to provide
    * span names.
    */
  override def and(that: AttributeProvider): SpanDataProvider =
    new SpanDataProvider {
      type Shared = self.Shared

      def processSharedData[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
      ): Shared =
        self.processSharedData(request, urlTemplateClassifier, urlRedactor)

      def spanName[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Shared,
      ): String =
        self.spanName(request, urlTemplateClassifier, urlRedactor, sharedProcessedData)

      def requestAttributes[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Shared,
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes =
        self.requestAttributes(
          request,
          urlTemplateClassifier,
          urlRedactor,
          sharedProcessedData,
          headersAllowedAsAttributes,
        ) ++
          that.requestAttributes(
            request,
            urlTemplateClassifier,
            urlRedactor,
            headersAllowedAsAttributes,
          )

      def responseAttributes[F[_]](
          response: Response[F],
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes =
        self.responseAttributes(response, headersAllowedAsAttributes) ++
          that.responseAttributes(response, headersAllowedAsAttributes)

      def exceptionAttributes(cause: Throwable): Attributes =
        self.exceptionAttributes(cause) ++ that.exceptionAttributes(cause)
    }
}

object SpanDataProvider {

  /** The default provider, which follows OpenTelemetry semantic conventions. */
  def default: SpanDataProvider = openTelemetry

  /** A `SpanAndAttributeProvider` following OpenTelemetry semantic conventions. */
  val openTelemetry: SpanDataProvider = {
    final case class Data(httpRequestMethod: Attribute[String]) {
      val requestMethodIsUnknown: Boolean =
        httpRequestMethod == TypedAttributes.httpRequestMethodOther
    }

    new SpanDataProvider {
      // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client

      type Shared = Data

      def processSharedData[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
      ): Data =
        Data(TypedAttributes.httpRequestMethod(request.method))

      def spanName[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Data,
      ): String = {
        val method =
          if (sharedProcessedData.requestMethodIsUnknown) "HTTP"
          else sharedProcessedData.httpRequestMethod.value
        urlTemplateClassifier
          .classify(request.uri)
          .fold(method)(urlTemplate => s"$method $urlTemplate")
      }

      def requestAttributes[F[_]](
          request: Request[F],
          urlTemplateClassifier: UriTemplateClassifier,
          urlRedactor: UriRedactor,
          sharedProcessedData: Data,
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes = {
        val b = Attributes.newBuilder

        b += sharedProcessedData.httpRequestMethod
        b ++= TypedClientAttributes.serverAddress(request.uri.host)
        b ++= TypedClientAttributes.serverPort(request.remotePort, request.uri)
        b += TypedClientAttributes.urlFull(request.uri, urlRedactor)
        // `error.type` handled by `responseAttributes` and `exceptionAttributes`
        if (sharedProcessedData.requestMethodIsUnknown) {
          b += TypedAttributes.httpRequestMethodOriginal(request.method)
        }
        // `http.response.status_code` handled by `responseAttributes`
        // `network.protocol.name` not required because http4s only supports http
        //   and `Request#httpVersion` is always populated/available
        b ++= TypedClientAttributes.httpRequestResendCount(request)
        request.remote.foreach { socketAddress =>
          b += TypedAttributes.networkPeerAddress(socketAddress.host)
          b += TypedAttributes.networkPeerPort(socketAddress.port)
        }
        b += TypedAttributes.networkProtocolVersion(request.httpVersion)
        TypedAttributes.httpRequestHeadersForBuilder(request.headers, headersAllowedAsAttributes)(b)
        // `http.response.header.<key>`s handled by `responseAttributes`
        // `network.transport` not opted into at this time
        b ++= TypedClientAttributes.urlScheme(request.uri.scheme)
        b ++= TypedAttributes.userAgentOriginal(request.headers)

        b.result()
      }

      def responseAttributes[F[_]](
          response: Response[F],
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes = {
        val b = Attributes.newBuilder

        if (!response.status.isSuccess) {
          // `error.type` for a `Throwable` handled by `exceptionAttributes`
          b += TypedAttributes.errorType(response.status)
        }
        b += TypedAttributes.httpResponseStatusCode(response.status)
        TypedAttributes.httpResponseHeadersForBuilder(response.headers, headersAllowedAsAttributes)(
          b
        )

        b.result()
      }

      def exceptionAttributes(cause: Throwable): Attributes =
        Attributes(TypedAttributes.errorType(cause))
    }
  }
}
