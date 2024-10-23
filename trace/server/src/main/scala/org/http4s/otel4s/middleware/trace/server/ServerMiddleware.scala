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
package server

import cats.data.Kleisli
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import org.http4s.Status.ServerError
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

import scala.collection.immutable

/** Middleware for wrapping an http4s `Server` to add tracing.
  *
  * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  */
object ServerMiddleware {

  /** @return a server middleware builder with default configuration */
  def default[F[_]: Tracer: MonadCancelThrow]: ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      Defaults.allowedRequestHeaders,
      Defaults.allowedResponseHeaders,
      Defaults.routeClassifier,
      Defaults.serverSpanName,
      Defaults.defaultRequestAttributesFilter,
      Defaults.defaultResponseAttributesFilter,
      Defaults.additionalRequestAttributes,
      Defaults.additionalResponseAttributes,
      Defaults.urlRedactor,
      Defaults.shouldTrace,
    )

  /** The default configuration values for a server middleware builder. */
  object Defaults {
    def allowedRequestHeaders: Set[CIString] =
      TypedAttributes.Headers.defaultAllowedHeaders
    def allowedResponseHeaders: Set[CIString] =
      TypedAttributes.Headers.defaultAllowedHeaders
    val routeClassifier: RequestPrelude => Option[String] = _ => None
    val serverSpanName: RequestPrelude => String =
      req => s"Http Server - ${req.method}"
    val defaultRequestAttributesFilter: Attribute[_] => Boolean = _ => true
    val defaultResponseAttributesFilter: Attribute[_] => Boolean = _ => true
    val additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]] =
      _ => Nil
    val additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]] =
      _ => Nil
    def urlRedactor: UriRedactor = UriRedactor.OnlyRedactUserInfo
    val shouldTrace: RequestPrelude => ShouldTrace = _ => ShouldTrace.Trace
  }

  /** A builder for server middlewares. */
  final class ServerMiddlewareBuilder[F[_]: Tracer: MonadCancelThrow] private[ServerMiddleware] (
      allowedRequestHeaders: Set[CIString],
      allowedResponseHeaders: Set[CIString],
      routeClassifier: RequestPrelude => Option[String],
      serverSpanName: RequestPrelude => String,
      defaultRequestAttributesFilter: Attribute[_] => Boolean,
      defaultResponseAttributesFilter: Attribute[_] => Boolean,
      additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]],
      additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]],
      urlRedactor: UriRedactor,
      shouldTrace: RequestPrelude => ShouldTrace,
  ) {
    private def copy(
        allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
        allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
        routeClassifier: RequestPrelude => Option[String] = this.routeClassifier,
        serverSpanName: RequestPrelude => String = this.serverSpanName,
        defaultRequestAttributesFilter: Attribute[_] => Boolean =
          this.defaultRequestAttributesFilter,
        defaultResponseAttributesFilter: Attribute[_] => Boolean =
          this.defaultResponseAttributesFilter,
        additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]] =
          this.additionalRequestAttributes,
        additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]] =
          this.additionalResponseAttributes,
        urlRedactor: UriRedactor = this.urlRedactor,
        shouldTrace: RequestPrelude => ShouldTrace = this.shouldTrace,
    ): ServerMiddlewareBuilder[F] =
      new ServerMiddlewareBuilder[F](
        allowedRequestHeaders,
        allowedResponseHeaders,
        routeClassifier,
        serverSpanName,
        defaultRequestAttributesFilter,
        defaultResponseAttributesFilter,
        additionalRequestAttributes,
        additionalResponseAttributes,
        urlRedactor,
        shouldTrace,
      )

    /** Sets which request headers are allowed to made into `Attribute`s. */
    def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(allowedRequestHeaders = allowedHeaders)

    /** Sets which response headers are allowed to made into `Attribute`s. */
    def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(allowedResponseHeaders = allowedHeaders)

    /** Sets how to determine the route within the application from a request.
      * A value of `None` returned by the given function indicates that the
      * route could not be determined.
      */
    def withRouteClassifier(
        routeClassifier: RequestPrelude => Option[String]
    ): ServerMiddlewareBuilder[F] =
      copy(routeClassifier = routeClassifier)

    /** Sets how to derive the name of a server span from a request. */
    def withServerSpanName(serverSpanName: RequestPrelude => String): ServerMiddlewareBuilder[F] =
      copy(serverSpanName = serverSpanName)

    /** Allows to filter default request attributes. */
    def withDefaultRequestAttributesFilter(
        defaultRequestAttributesFilter: Attribute[_] => Boolean
    ): ServerMiddlewareBuilder[F] =
      copy(defaultRequestAttributesFilter = defaultRequestAttributesFilter)

    /** Allows to filter default response attributes. */
    def withDefaultResponseAttributesFilter(
        defaultResponseAttributesFilter: Attribute[_] => Boolean
    ): ServerMiddlewareBuilder[F] =
      copy(defaultResponseAttributesFilter = defaultResponseAttributesFilter)

    /** Sets how to derive additional `Attribute`s from a request to add to the
      *  server span.
      */
    def withAdditionalRequestAttributes(
        additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalRequestAttributes = additionalRequestAttributes)

    /** Sets how to derive additional `Attribute`s from a response to add to the
      *  server span.
      */
    def withAdditionalResponseAttributes(
        additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalResponseAttributes = additionalResponseAttributes)

    /** Sets how to redact URLs before turning them into `Attribute`s. */
    def withUrlRedactor(urlRedactor: UriRedactor): ServerMiddlewareBuilder[F] =
      copy(urlRedactor = urlRedactor)

    /** Sets how to determine when to trace a request and its response. */
    def withShouldTrace(shouldTrace: RequestPrelude => ShouldTrace): ServerMiddlewareBuilder[F] =
      copy(shouldTrace = shouldTrace)

    /** Returns a middleware in a way that abstracts over
      * [[org.http4s.HttpApp `HttpApp`]] and
      * [[org.http4s.HttpRoutes `HttpRoutes`]]. In most cases, it is preferable
      * to use the methods that directly build the specific desired type.
      *
      * @see [[buildHttpApp]]
      * @see [[buildHttpRoutes]]
      */
    def buildGenericTracedHttp[G[_]: MonadCancelThrow](
        f: Http[G, F]
    )(implicit kt: KindTransformer[F, G]): Http[G, F] =
      Kleisli { (req: Request[F]) =>
        val reqPrelude = req.requestPrelude
        if (
          !shouldTrace(reqPrelude).shouldTrace ||
          !Tracer[F].meta.isEnabled
        ) {
          f(req)
        } else {
          val init =
            request(
              req,
              allowedRequestHeaders,
              routeClassifier,
              urlRedactor,
              defaultRequestAttributesFilter,
            ) ++ additionalRequestAttributes(reqPrelude)
          MonadCancelThrow[G].uncancelable { poll =>
            val tracerG = Tracer[F].mapK[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG
                .spanBuilder(serverSpanName(reqPrelude))
                .withSpanKind(SpanKind.Server)
                .addAttributes(init)
                .build
                .use { span =>
                  poll(f.run(req))
                    .guaranteeCase {
                      case Outcome.Succeeded(fa) =>
                        fa.flatMap { resp =>
                          val out =
                            response(
                              resp,
                              allowedResponseHeaders,
                              defaultResponseAttributesFilter,
                            ) ++ additionalResponseAttributes(resp.responsePrelude)

                          span.addAttributes(out) >> span
                            .setStatus(StatusCode.Error)
                            .unlessA(resp.status.isSuccess)
                        }
                      case Outcome.Errored(e) =>
                        span.addAttributes(TypedAttributes.errorType(e))
                      case Outcome.Canceled() =>
                        MonadCancelThrow[G].unit
                    }
                }
            }
          }
        }
      }

    /** @return a configured middleware for `HttpApp` */
    def buildHttpApp(f: HttpApp[F]): HttpApp[F] =
      buildGenericTracedHttp(f)

    /** @return a configured middleware for `HttpRoutes` */
    def buildHttpRoutes(f: HttpRoutes[F]): HttpRoutes[F] =
      buildGenericTracedHttp(f)
  }

  /** @return the default `Attribute`s for a request */
  private def request[F[_]](
      request: Request[F],
      allowedHeaders: Set[CIString],
      routeClassifier: RequestPrelude => Option[String],
      urlRedactor: UriRedactor,
      defaultRequestAttributesFilter: Attribute[_] => Boolean,
  ): Attributes = {
    val builder = Attributes.newBuilder
    builder += TypedAttributes.httpRequestMethod(request.method)
    builder ++= TypedAttributes.url(request.uri, urlRedactor)
    val host = request.headers.get[Host].getOrElse {
      val authority = request.uri.authority.getOrElse(Uri.Authority())
      Host(authority.host.value, authority.port)
    }
    builder += TypedAttributes.serverAddress(host)
    request.headers
      .get[`User-Agent`]
      .foreach(ua => builder += TypedAttributes.userAgentOriginal(ua))

    routeClassifier(request.requestPrelude).foreach(route =>
      builder += TypedServerAttributes.httpRoute(route)
    )

    request.remote.foreach { socketAddress =>
      builder +=
        TypedAttributes.networkPeerAddress(socketAddress.host)

      builder +=
        TypedServerAttributes.clientPort(socketAddress.port)
    }

    TypedServerAttributes.clientAddress(request).foreach(builder += _)
    builder ++=
      TypedAttributes.Headers.request(request.headers, allowedHeaders)

    builder.result().filter(defaultRequestAttributesFilter)
  }

  /** @return the default `Attribute`s for a response */
  private def response[F[_]](
      response: Response[F],
      allowedHeaders: Set[CIString],
      defaultResponseAttributesFilter: Attribute[_] => Boolean,
  ): Attributes = {
    val builder = Attributes.newBuilder

    builder += TypedAttributes.httpResponseStatusCode(response.status)
    builder ++= TypedAttributes.Headers.response(response.headers, allowedHeaders)

    // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server-semantic-conventions
    // [5]: If response status code was sent or received and status indicates an error according
    // to HTTP span status definition, `error.type` SHOULD be set to the status code number (represented as a string),
    // an exception type (if thrown) or a component-specific error identifier.
    //
    // For HTTP status codes in the 4xx range span status MUST be left unset in case of SpanKind.SERVER
    // and SHOULD be set to Error in case of SpanKind.CLIENT.
    // For HTTP status codes in the 5xx range, as well as any other code the client failed to interpret,
    // span status SHOULD be set to Error.
    if (response.status.responseClass == ServerError)
      builder += TypedAttributes.errorType(response.status)

    builder.result().filter(defaultResponseAttributesFilter)
  }
}
