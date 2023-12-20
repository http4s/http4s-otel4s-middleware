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

package org.http4s.otel4s.middleware

import cats.data.Kleisli
import cats.effect.kernel.MonadCancelThrow
import cats.effect.kernel.Outcome
import cats.effect.syntax.monadCancel._
import cats.syntax.flatMap._
import org.http4s.Http
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.RequestPrelude
import org.http4s.Response
import org.http4s.client.RequestKey
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer

object ServerMiddleware {

  def default[F[_]: Tracer: MonadCancelThrow]: ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](
      Defaults.isKernelHeader,
      Defaults.reqHeaders,
      Defaults.respHeaders,
      Defaults.routeClassifier,
      Defaults.serverSpanName,
      Defaults.additionalRequestTags,
      Defaults.additionalResponseTags,
      Defaults.includeUrl,
      Defaults.doNotTrace,
    )

  object Defaults {
    val isKernelHeader: CIString => Boolean = name => !ExcludedHeaders.contains(name)
    val reqHeaders: Set[CIString] = HttpAttributes.Headers.defaultAllowedHeaders
    val respHeaders: Set[CIString] = HttpAttributes.Headers.defaultAllowedHeaders
    def routeClassifier[F[_]]: Request[F] => Option[String] = { (_: Request[F]) => None }
    def serverSpanName[F[_]]: Request[F] => String = { (req: Request[F]) =>
      s"Http Server - ${req.method}"
    }
    def additionalRequestTags[F[_]]: Request[F] => Seq[Attribute[_]] = { (_: Request[F]) => Seq() }
    def additionalResponseTags[F[_]]: Response[F] => Seq[Attribute[_]] = { (_: Response[F]) =>
      Seq()
    }
    def includeUrl[F[_]]: Request[F] => Boolean = { (_: Request[F]) => true }
    def doNotTrace: RequestPrelude => Boolean = { (_: RequestPrelude) => false }
  }

  final class ServerMiddlewareBuilder[F[_]: Tracer: MonadCancelThrow] private[ServerMiddleware] (
      isKernelHeader: CIString => Boolean,
      reqHeaders: Set[CIString],
      respHeaders: Set[CIString],
      routeClassifier: Request[F] => Option[String],
      serverSpanName: Request[F] => String,
      additionalRequestTags: Request[F] => Seq[Attribute[_]],
      additionalResponseTags: Response[F] => Seq[Attribute[_]],
      includeUrl: Request[F] => Boolean,
      doNotTrace: RequestPrelude => Boolean,
  ) { self =>

    private def copy(
        isKernelHeader: CIString => Boolean = self.isKernelHeader,
        reqHeaders: Set[CIString] = self.reqHeaders,
        respHeaders: Set[CIString] = self.respHeaders,
        routeClassifier: Request[F] => Option[String] = self.routeClassifier,
        serverSpanName: Request[F] => String = self.serverSpanName,
        additionalRequestTags: Request[F] => Seq[Attribute[_]] = self.additionalRequestTags,
        additionalResponseTags: Response[F] => Seq[Attribute[_]] = self.additionalResponseTags,
        includeUrl: Request[F] => Boolean = self.includeUrl,
        doNotTrace: RequestPrelude => Boolean = self.doNotTrace,
    ): ServerMiddlewareBuilder[F] =
      new ServerMiddlewareBuilder[F](
        isKernelHeader,
        reqHeaders,
        respHeaders,
        routeClassifier,
        serverSpanName,
        additionalRequestTags,
        additionalResponseTags,
        includeUrl,
        doNotTrace,
      )

    def withIsKernelHeader(isKernelHeader: CIString => Boolean): ServerMiddlewareBuilder[F] =
      copy(isKernelHeader = isKernelHeader)
    def withRequestHeaders(reqHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(reqHeaders = reqHeaders)
    def withResponseHeaders(respHeaders: Set[CIString]): ServerMiddlewareBuilder[F] =
      copy(respHeaders = respHeaders)
    def withRouteClassifier(
        routeClassifier: Request[F] => Option[String]
    ): ServerMiddlewareBuilder[F] =
      copy(routeClassifier = routeClassifier)
    def withServerSpanName(serverSpanName: Request[F] => String): ServerMiddlewareBuilder[F] =
      copy(serverSpanName = serverSpanName)
    def withAdditionalRequestTags(
        additionalRequestTags: Request[F] => Seq[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalRequestTags = additionalRequestTags)
    def withAdditionalResponseTags(
        additionalResponseTags: Response[F] => Seq[Attribute[_]]
    ): ServerMiddlewareBuilder[F] =
      copy(additionalResponseTags = additionalResponseTags)
    def withIncludeUrl(includeUrl: Request[F] => Boolean): ServerMiddlewareBuilder[F] =
      copy(includeUrl = includeUrl)
    def withDoNotTrace(doNotTrace: RequestPrelude => Boolean): ServerMiddlewareBuilder[F] =
      copy(doNotTrace = doNotTrace)

    private def buildTracedF[G[_]: MonadCancelThrow](
        f: Http[G, F]
    )(implicit kt: KindTransformer[F, G]): Http[G, F] =
      Kleisli { (req: Request[F]) =>
        if (doNotTrace(req.requestPrelude)) f(req)
        else {
          val init =
            request(req, reqHeaders, routeClassifier, includeUrl) ++ additionalRequestTags(req)
          MonadCancelThrow[G].uncancelable { poll =>
            val tracerG = Tracer[F].mapK[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG
                .spanBuilder(serverSpanName(req))
                .withSpanKind(SpanKind.Server)
                .addAttributes(init: _*)
                .build
                .use { span =>
                  poll(f.run(req))
                    .guaranteeCase {
                      case Outcome.Succeeded(fa) =>
                        span.addAttribute(Attribute("exit.case", "succeeded")) >>
                          fa.flatMap { resp =>
                            val out = response(resp, respHeaders) ++ additionalResponseTags(resp)
                            span.addAttributes(out: _*)
                          }
                      case Outcome.Errored(e) =>
                        span.recordException(e) >>
                          span.addAttribute(Attribute("exit.case", "errored"))
                      case Outcome.Canceled() =>
                        span.addAttributes(
                          Attribute("exit.case", "canceled"),
                          Attribute("canceled", true),
                          Attribute(
                            "error",
                            true,
                          ), // A canceled http is an error for the server. The connection got cut for some reason.
                        )
                    }
                }
            }
          }
        }
      }

    def buildHttpApp(f: HttpApp[F]): HttpApp[F] =
      buildTracedF(f)

    def buildHttpRoutes(f: HttpRoutes[F]): HttpRoutes[F] =
      buildTracedF(f)
  }

  private[middleware] def request[F[_]](
      req: Request[F],
      headers: Set[CIString],
      routeClassifier: Request[F] => Option[String],
  ): List[Attribute[_]] =
    request(req, headers, routeClassifier, Function.const[Boolean, Request[F]](true))

  def request[F[_]](
      request: Request[F],
      allowedHeaders: Set[CIString],
      routeClassifier: Request[F] => Option[String],
      includeUrl: Request[F] => Boolean,
  ): List[Attribute[_]] = {
    val builder = List.newBuilder[Attribute[_]]
    builder += HttpAttributes.httpRequestMethod(request.method)
    if (includeUrl(request)) {
      builder += HttpAttributes.urlFull(request.uri)
      builder += HttpAttributes.urlPath(request.uri.path)
      builder += HttpAttributes.urlQuery(request.uri.query)
    }
    val host = request.headers.get[Host].getOrElse {
      val key = RequestKey.fromRequest(request)
      Host(key.authority.host.value, key.authority.port)
    }
    builder += HttpAttributes.serverAddress(host)
    request.uri.scheme.foreach(s => builder += HttpAttributes.urlScheme(s))
    request.headers.get[`User-Agent`].foreach(ua => builder += HttpAttributes.userAgentOriginal(ua))

    routeClassifier(request).foreach(route => builder += HttpAttributes.Server.httpRoute(route))

    request.remote.foreach { socketAddress =>
      builder +=
        HttpAttributes.networkPeerAddress(socketAddress.host)

      builder +=
        HttpAttributes.Server.clientPort(socketAddress.port)
    }

    HttpAttributes.Server.clientAddress(request).foreach(builder += _)
    builder ++=
      HttpAttributes.Headers.request(request.headers, allowedHeaders)

    builder.result()
  }

  def response[F[_]](response: Response[F], allowedHeaders: Set[CIString]): List[Attribute[_]] = {
    val builder = List.newBuilder[Attribute[_]]

    builder += HttpAttributes.httpResponseStatusCode(response.status)
    builder ++= HttpAttributes.Headers.response(response.headers, allowedHeaders)

    builder.result()
  }

  val ExcludedHeaders: Set[CIString] = {
    import org.http4s.headers._
    import org.typelevel.ci._

    val payload = Set(
      `Content-Length`.name,
      ci"Content-Type",
      `Content-Range`.name,
      ci"Trailer",
      `Transfer-Encoding`.name,
    )

    val security = Set(
      Authorization.name,
      Cookie.name,
      `Set-Cookie`.name,
    )

    payload ++ security
  }

}
