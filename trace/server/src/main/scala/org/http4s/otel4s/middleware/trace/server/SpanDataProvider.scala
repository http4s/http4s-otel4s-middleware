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
package trace.server

import org.http4s.headers.Forwarded
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

  /** Process data used to provide both the span name and request `Attributes`.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def processSharedData[F[_]](
      request: Request[F],
      routeClassifier: RouteClassifier,
      redactor: PathAndQueryRedactor,
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
      routeClassifier: RouteClassifier,
      redactor: PathAndQueryRedactor,
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
      routeClassifier: RouteClassifier,
      redactor: PathAndQueryRedactor,
      sharedProcessedData: Shared,
      headersAllowedAsAttributes: Set[CIString],
  ): Attributes

  final def requestAttributes[F[_]](
      request: Request[F],
      routeClassifier: RouteClassifier,
      redactor: PathAndQueryRedactor,
      headersAllowedAsAttributes: Set[CIString],
  ): Attributes =
    requestAttributes(
      request,
      routeClassifier,
      redactor,
      processSharedData(request, routeClassifier, redactor),
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
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
      ): Shared =
        self.processSharedData(request, routeClassifier, redactor)

      def spanName[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
          sharedProcessedData: Shared,
      ): String =
        self.spanName(request, routeClassifier, redactor, sharedProcessedData)

      def requestAttributes[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
          sharedProcessedData: Shared,
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes =
        self.requestAttributes(
          request,
          routeClassifier,
          redactor,
          sharedProcessedData,
          headersAllowedAsAttributes,
        ) ++
          that.requestAttributes(
            request,
            routeClassifier,
            redactor,
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
    final case class Data(
        httpRequestMethod: Attribute[String],
        httpRoute: Option[Attribute[String]],
    ) {
      val requestMethodIsUnknown: Boolean =
        httpRequestMethod == TypedAttributes.httpRequestMethodOther
    }

    new SpanDataProvider {
      // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server-semantic-conventions

      type Shared = Data

      def processSharedData[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
      ): Data =
        Data(
          httpRequestMethod = TypedAttributes.httpRequestMethod(request.method),
          httpRoute = TypedServerAttributes.httpRoute(request, routeClassifier),
        )

      def spanName[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
          sharedProcessedData: Data,
      ): String = {
        val method =
          if (sharedProcessedData.requestMethodIsUnknown) "HTTP"
          else sharedProcessedData.httpRequestMethod.value
        sharedProcessedData.httpRoute
          .fold(method)(attr => s"$method ${attr.value}")
      }

      def requestAttributes[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
          sharedProcessedData: Data,
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes = {
        val b = Attributes.newBuilder
        val forwarded = request.headers.get[Forwarded]
        val scheme = OriginalScheme(forwarded, request.headers, request.uri)

        b += sharedProcessedData.httpRequestMethod // http4s does not support unknown request methods
        b ++= TypedServerAttributes.urlPath(request.uri.path, redactor)
        b ++= TypedServerAttributes.urlScheme(scheme)
        // `error.type` handled by `responseAttributes` and `exceptionAttributes`
        if (sharedProcessedData.requestMethodIsUnknown) {
          b += TypedAttributes.httpRequestMethodOriginal(request.method)
        }
        // `http.response.status_code` handled by `responseAttributes`
        b ++= sharedProcessedData.httpRoute
        // `network.protocol.name` not required because http4s only supports http
        //   and `Request#httpVersion` is always populated/available
        // `server.port` handled later with server.address
        b ++= TypedServerAttributes.urlQuery(request.uri.query, redactor)
        // `client.port` handled here (see below)
        TypedServerAttributes.clientAddressAndPortForBuilder(request, forwarded)(b)
        request.remote.foreach { socketAddress =>
          b += TypedAttributes.networkPeerAddress(socketAddress.host)
          b += TypedAttributes.networkPeerPort(socketAddress.port)
        }
        b += TypedAttributes.networkProtocolVersion(request.httpVersion)
        // `server.port` handled here (see above)
        TypedServerAttributes.serverAddressAndPortForBuilder(request, forwarded, scheme)(b)
        b ++= TypedAttributes.userAgentOriginal(request.headers)
        // `client.port` handled earlier with client.address
        TypedAttributes.httpRequestHeadersForBuilder(request.headers, headersAllowedAsAttributes)(b)
        // `http.response.header.<key>`s handled by `responseAttributes`
        // `network.local.address` not opted into at this time
        // `network.local.port` not opted into at this time
        // `network.transport` not opted into at this time

        b.result()
      }

      def responseAttributes[F[_]](
          response: Response[F],
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes = {
        val b = Attributes.newBuilder

        if (response.status.responseClass == Status.ServerError) {
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
