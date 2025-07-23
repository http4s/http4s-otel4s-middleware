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
package server

import org.http4s.headers.Forwarded
import org.http4s.otel4s.middleware.server.OriginalScheme
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

/** Utility for creating built-in [[`SpanDataProvider`]]s for a server. */
object ServerSpanDataProvider {

  /** Opt-In `Attribute` configuration. */
  private[this] final case class OptIn(
      clientPort: Boolean = false,
      httpRequestHeaders: Option[HeaderRedactor] = None,
      httpResponseHeaders: Option[HeaderRedactor] = None,
  )

  /** Data shared by OpenTelemetry span name and request `Attributes`. */
  private[this] final case class OtelData(
      httpRequestMethod: Attribute[String],
      httpRoute: Option[Attribute[String]],
  ) {
    val requestMethodIsUnknown: Boolean =
      httpRequestMethod == TypedServerTraceAttributes.httpRequestMethodOther
  }

  /** An immutable builder for configuring a [[`SpanDataProvider`]] consistent
    * with OpenTelemetry semantic conventions.
    */
  sealed trait OpenTelemetryBuilder { self: SpanDataProvider =>

    /** Sets how to determine the route within the application from a request. */
    def withRouteClassifier(
        routeClassifier: RouteClassifier
    ): SpanDataProvider with OpenTelemetryBuilder

    /** Opts into providing the `client.port` `Attribute` for spans. */
    def optIntoClientPort: SpanDataProvider with OpenTelemetryBuilder

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
  }

  // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server-semantic-conventions
  private[this] final class OtelProvider(
      pathAndQueryRedactor: PathAndQueryRedactor,
      routeClassifier: RouteClassifier,
      optIn: OptIn,
  ) extends SpanDataProvider
      with OpenTelemetryBuilder {
    type Shared = OtelData

    def processSharedData[F[_]](request: Request[F]): OtelData =
      OtelData(
        httpRequestMethod = TypedServerTraceAttributes.httpRequestMethod(request.method),
        httpRoute = TypedServerTraceAttributes.httpRoute(request, routeClassifier),
      )

    def spanName[F[_]](request: Request[F], sharedProcessedData: OtelData): String = {
      val method =
        if (sharedProcessedData.requestMethodIsUnknown) "HTTP"
        else sharedProcessedData.httpRequestMethod.value
      sharedProcessedData.httpRoute
        .fold(method)(attr => s"$method ${attr.value}")
    }

    def requestAttributes[F[_]](request: Request[F], sharedProcessedData: OtelData): Attributes = {
      val b = Attributes.newBuilder
      val forwarded = request.headers.get[Forwarded]
      val scheme = OriginalScheme(forwarded, request.headers, request.uri)

      b += sharedProcessedData.httpRequestMethod
      b ++= TypedServerTraceAttributes.urlPath(request.uri.path, pathAndQueryRedactor)
      b ++= TypedServerTraceAttributes.urlScheme(scheme)
      // `error.type` handled by `responseAttributes` and `exceptionAttributes`
      if (sharedProcessedData.requestMethodIsUnknown) {
        b += TypedServerTraceAttributes.httpRequestMethodOriginal(request.method)
      }
      // `http.response.status_code` handled by `responseAttributes`
      b ++= sharedProcessedData.httpRoute
      // `network.protocol.name` not required because http4s only supports http
      //   and `Request#httpVersion` is always populated/available
      // `server.port` handled later with server.address
      b ++= TypedServerTraceAttributes.urlQuery(request.uri.query, pathAndQueryRedactor)
      // `client.port` handled here (see below)
      if (optIn.clientPort) {
        TypedServerTraceAttributes.clientAddressAndPortForBuilder(request, forwarded)(b)
      } else b ++= TypedServerTraceAttributes.clientAddress(request, forwarded)
      request.remote.foreach { socketAddress =>
        b += TypedServerTraceAttributes.networkPeerAddress(socketAddress.host)
        b += TypedServerTraceAttributes.networkPeerPort(socketAddress.port)
      }
      b += TypedServerTraceAttributes.networkProtocolVersion(request.httpVersion)
      // `server.port` handled here (see above)
      TypedServerTraceAttributes.serverAddressAndPortForBuilder(request, forwarded, scheme)(b)
      b ++= TypedServerTraceAttributes.userAgentOriginal(request.headers)
      // `client.port` handled earlier with `client.address`
      // `http.request.body.size` not available to opt into at this time
      optIn.httpRequestHeaders.foreach { redactor =>
        TypedServerTraceAttributes
          .httpRequestHeadersForBuilder(request.headers, redactor)(b)
      }
      // `http.request.size` not available to opt into at this time
      // `http.response.body.size` not available to opt into at this time
      // `http.response.header.<key>`s handled by `responseAttributes`
      // `http.response.size` not available to opt into at this time
      // `network.local.address` not available to opt into at this time
      // `network.local.port` not available to opt into at this time
      // `network.transport` not available to opt into at this time
      // `user_agent.synthetic.type` not available to opt into at this time

      b.result()
    }

    def responseAttributes[F[_]](response: Response[F]): Attributes = {
      val b = Attributes.newBuilder

      if (response.status.responseClass == Status.ServerError) {
        // `error.type` for a `Throwable` handled by `exceptionAttributes`
        b += TypedServerTraceAttributes.errorType(response.status)
      }
      b += TypedServerTraceAttributes.httpResponseStatusCode(response.status)
      optIn.httpResponseHeaders.foreach { redactor =>
        TypedServerTraceAttributes
          .httpResponseHeadersForBuilder(response.headers, redactor)(b)
      }

      b.result()
    }

    def exceptionAttributes(cause: Throwable): Attributes =
      Attributes(TypedServerTraceAttributes.errorType(cause))

    private[this] def copy(
        routeClassifier: RouteClassifier = this.routeClassifier,
        optIn: OptIn = this.optIn,
    ): OtelProvider =
      new OtelProvider(
        this.pathAndQueryRedactor,
        routeClassifier,
        optIn,
      )
    def withRouteClassifier(routeClassifier: RouteClassifier): OtelProvider =
      copy(routeClassifier = routeClassifier)
    def optIntoClientPort: OtelProvider =
      copy(optIn = optIn.copy(clientPort = true))
    def optIntoHttpRequestHeaders(redactor: HeaderRedactor): OtelProvider =
      copy(optIn = optIn.copy(httpRequestHeaders = Some(redactor)))
    def optIntoHttpResponseHeaders(redactor: HeaderRedactor): OtelProvider =
      copy(optIn = optIn.copy(httpResponseHeaders = Some(redactor)))
  }

  /** @param redactor a redactor with which to sanitize requests' path and
    *                 query before using them as values for `Attribute`s.
    * @return a configurable [[`SpanDataProvider`]] following OpenTelemetry
    *         semantic conventions
    * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
    */
  def openTelemetry(redactor: PathAndQueryRedactor): SpanDataProvider with OpenTelemetryBuilder =
    new OtelProvider(redactor, RouteClassifier.indeterminate, OptIn())
}
