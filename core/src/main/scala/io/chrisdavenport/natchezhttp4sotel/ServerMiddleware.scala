package io.chrisdavenport.natchezhttp4sotel

import cats.data.Kleisli
import cats.effect.kernel.{MonadCancelThrow, Outcome}
import cats.effect.syntax.all._
import cats.syntax.all._
import org.http4s.client.RequestKey
import org.http4s.headers.{Host, `User-Agent`}
import org.http4s._
import org.typelevel.ci.CIString
import org.typelevel.otel4s.{Attribute, KindTransformer}
import org.typelevel.otel4s.trace.Tracer

object ServerMiddleware {

  def default[F[_]: Tracer: MonadCancelThrow]: ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](Defaults.isKernelHeader, Defaults.reqHeaders, Defaults.respHeaders, Defaults.routeClassifier, Defaults.serverSpanName, Defaults.additionalRequestTags, Defaults.additionalResponseTags, Defaults.includeUrl, Defaults.doNotTrace)

  object Defaults {
    val isKernelHeader: CIString => Boolean = name => !ExcludedHeaders.contains(name)
    val reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded
    val respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded
    def routeClassifier[F[_]]: Request[F] => Option[String] = {(_: Request[F]) => None}
    def serverSpanName[F[_]]: Request[F] => String = {(req: Request[F]) => s"Http Server - ${req.method}"}
    def additionalRequestTags[F[_]]: Request[F] => Seq[Attribute[_]] = {(_: Request[F]) => Seq()}
    def additionalResponseTags[F[_]]: Response[F] => Seq[Attribute[_]] = {(_: Response[F]) => Seq()}
    def includeUrl[F[_]]: Request[F] => Boolean = {(_: Request[F]) => true}
    def doNotTrace: RequestPrelude => Boolean = {(_: RequestPrelude) => false}
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
  ){ self =>

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
      new ServerMiddlewareBuilder[F](isKernelHeader, reqHeaders, respHeaders, routeClassifier, serverSpanName, additionalRequestTags, additionalResponseTags, includeUrl, doNotTrace)

    def withIsKernelHeader(isKernelHeader: CIString => Boolean) = copy(isKernelHeader = isKernelHeader)
    def withRequestHeaders(reqHeaders: Set[CIString]) = copy(reqHeaders = reqHeaders)
    def withResponseHeaders(respHeaders: Set[CIString]) = copy(respHeaders = respHeaders)
    def withRouteClassifier(routeClassifier: Request[F] => Option[String]) = copy(routeClassifier = routeClassifier)
    def withServerSpanName(serverSpanName: Request[F] => String) = copy(serverSpanName = serverSpanName)
    def withAdditionalRequestTags(additionalRequestTags: Request[F] => Seq[Attribute[_]]) = copy(additionalRequestTags = additionalRequestTags)
    def withAdditionalResponseTags(additionalResponseTags: Response[F] => Seq[Attribute[_]]) = copy(additionalResponseTags = additionalResponseTags)
    def withIncludeUrl(includeUrl: Request[F] => Boolean) = copy(includeUrl = includeUrl)
    def withDoNotTrace(doNotTrace: RequestPrelude => Boolean) = copy(doNotTrace = doNotTrace)


    private def buildTracedF[G[_]: MonadCancelThrow](f: Http[G, F])(implicit kt: KindTransformer[F, G]): Http[G, F] =
      Kleisli { (req: Request[F]) =>
        if (doNotTrace(req.requestPrelude)) f(req)
        else {
          val init = request(req, reqHeaders, routeClassifier, includeUrl) ++ additionalRequestTags(req)
          MonadCancelThrow[G].uncancelable { poll =>
            val tracerG = Tracer[F].mapK[G]
            tracerG.joinOrRoot(req.headers) {
              tracerG.span(serverSpanName(req), init: _*).use { span =>
                poll(f.run(req))
                  .guaranteeCase {
                    case Outcome.Succeeded(fa) =>
                      span.addAttribute(Attribute("exit.case", "succeeded")) >>
                        fa.flatMap { resp =>
                          val out = response(resp, respHeaders) ++ additionalResponseTags(resp)
                          span.addAttributes(out: _*)
                        }
                    case Outcome.Errored(e) =>
                      span.addAttribute(Attribute("exit.case", "errored")) >>
                        span.addAttributes(OTHttpTags.Errors.error(e): _*)
                    case Outcome.Canceled() =>
                      span.addAttributes(
                        Attribute("exit.case", "canceled"),
                        Attribute("canceled", true),
                        Attribute("error", true) // A cancelled http is an error for the server. The connection got cut for some reason.
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

  private[natchezhttp4sotel] def request[F[_]](req: Request[F], headers: Set[CIString], routeClassifier: Request[F] => Option[String]): List[Attribute[_]] = {
    request(req, headers, routeClassifier, Function.const[Boolean, Request[F]](true))
  }

  def request[F[_]](request: Request[F], headers: Set[CIString], routeClassifier: Request[F] => Option[String], includeUrl: Request[F] => Boolean): List[Attribute[_]] = {
    val builder = List.newBuilder[Attribute[_]]
    builder += OTHttpTags.Common.kind("server")
    builder += OTHttpTags.Common.method(request.method)
    if (includeUrl(request)) {
      builder += OTHttpTags.Common.url(request.uri)
      builder += OTHttpTags.Common.target(request.uri)
    }
    val host = request.headers.get[Host].getOrElse{
      val key = RequestKey.fromRequest(request)
      Host(key.authority.host.value, key.authority.port)
    }
    builder += OTHttpTags.Common.host(host)
    request.uri.scheme.foreach( s => 
      builder += OTHttpTags.Common.scheme(s)
    )
    request.headers.get[`User-Agent`].foreach( ua => 
      builder += OTHttpTags.Common.userAgent(ua)
    )

    request.contentLength.foreach(l => 
      builder += OTHttpTags.Common.requestContentLength(l)
    )
    routeClassifier(request).foreach(s =>
      builder += OTHttpTags.Server.route(s)
    )

    builder += OTHttpTags.Common.flavor(request.httpVersion)

    request.remote.foreach{sa =>
      builder += 
        OTHttpTags.Common.peerIp(sa.host)

      builder += 
        OTHttpTags.Common.peerPort(sa.port)
    }
    // Special Server
    request.from.foreach(ip => 
      builder += OTHttpTags.Server.clientIp(ip)
    )
    builder ++= 
      OTHttpTags.Headers.request(request.headers, headers)

    builder.result()
  }

  def response[F[_]](response: Response[F], headers: Set[CIString]): List[Attribute[_]] = {
    val builder = List.newBuilder[Attribute[_]]

    builder += OTHttpTags.Common.status(response.status)
    response.contentLength.foreach(l => 
      builder += OTHttpTags.Common.responseContentLength(l)
    )
    builder ++=
      OTHttpTags.Headers.response(response.headers, headers)

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