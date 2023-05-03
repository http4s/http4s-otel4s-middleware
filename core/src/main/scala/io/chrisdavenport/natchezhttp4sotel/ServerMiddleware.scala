package io.chrisdavenport.natchezhttp4sotel

import cats._
import cats.syntax.all._
import cats.effect.kernel._
import cats.effect.syntax.all._
import org.http4s._
import org.typelevel.ci.CIString
import scala.collection.mutable.ListBuffer
import org.http4s.headers._
import org.http4s.client._
import io.chrisdavenport.fiberlocal._
import cats.data.OptionT
import java.net.URI
import cats.arrow.FunctionK
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import org.typelevel.otel4s.trace.SpanContext
import scodec.bits.ByteVector
import org.typelevel.otel4s.trace.SamplingDecision
import cats.effect.kernel.CancelScope.Uncancelable

object ServerMiddleware {

  def default[F[_]: Tracer: MonadCancelThrow]: ServerMiddlewareBuilder[F] =
    new ServerMiddlewareBuilder[F](Defaults.isKernelHeader, Defaults.reqHeaders, Defaults.respHeaders, Defaults.routeClassifier, Defaults.serverSpanName, Defaults.additionalRequestTags, Defaults.additionalResponseTags, Defaults.includeUrl, Function.const(false))

  object Defaults {
    val isKernelHeader: CIString => Boolean = name => !ExcludedHeaders.contains(name)
    val reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded
    val respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded
    def routeClassifier[F[_]]: Request[F] => Option[String] = {(_: Request[F]) => None}
    def serverSpanName[F[_]]: Request[F] => String = {(req: Request[F]) => s"Http Server - ${req.method}"}
    def additionalRequestTags[F[_]]: Request[F] => Seq[Attribute[_]] = {(_: Request[F]) => Seq()}
    def additionalResponseTags[F[_]]: Response[F] => Seq[Attribute[_]] = {(_: Response[F]) => Seq()}
    def includeUrl[F[_]]: Request[F] => Boolean = {(_: Request[F]) => true}
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


    final class MakeSureYouKnowWhatYouAreDoing{


      def buildTracedF[G[_]: MonadCancelThrow: Tracer](fk: F ~> G)(f: Http[G, F]): Http[G, F] = {
        cats.data.Kleisli{(req: Request[F]) =>
          if (doNotTrace(req.requestPrelude)) f(req)
          else {
            val init = request(req, reqHeaders, routeClassifier, includeUrl) ++ additionalRequestTags(req)
            MonadCancelThrow[G].uncancelable( poll =>
              Tracer[G].joinOrRoot(req.headers){
                Tracer[G].span(serverSpanName(req), init:_*).use{span =>
                  poll(f.run(req))
                    .guaranteeCase{
                      case Outcome.Succeeded(fa) =>
                        span.addAttribute(Attribute("exit.case","succeeded")) >>
                        fa.flatMap{resp =>
                          val out = response(resp, respHeaders) ++ additionalResponseTags(resp)
                          span.addAttributes(out:_*)
                        }
                      case Outcome.Errored(e) =>
                        span.addAttribute(Attribute("exit.case", "errored")) >>
                        span.addAttributes(OTHttpTags.Errors.error(e):_*)
                      case Outcome.Canceled() =>
                        span.addAttributes(
                          Attribute("exit.case", "canceled"),
                          Attribute("canceled", true),
                          Attribute("error", true) // A cancelled http is an error for the server. The connection got cut for some reason.
                        )
                    }

                }
              }(helpers.textMapGetter)
            )
          }
        }
      }
    }

    def MakeSureYouKnowWhatYouAreDoing = new MakeSureYouKnowWhatYouAreDoing()

    def buildHttpApp(f: HttpApp[F]): HttpApp[F] =
      MakeSureYouKnowWhatYouAreDoing.buildTracedF(FunctionK.id)(f)

    // def buildHttpRoutes(f: HttpRoutes[F]): HttpRoutes[F] =
    //   MakeSureYouKnowWhatYouAreDoing.buildTracedF(OptionT.liftK)(f)

  }


  private[natchezhttp4sotel] def request[F[_]](req: Request[F], headers: Set[CIString], routeClassifier: Request[F] => Option[String]): List[Attribute[_]] = {
    request(req, headers, routeClassifier, Function.const[Boolean, Request[F]](true))
  }

  def request[F[_]](request: Request[F], headers: Set[CIString], routeClassifier: Request[F] => Option[String], includeUrl: Request[F] => Boolean): List[Attribute[_]] = {
    val builder = new ListBuffer[Attribute[_]]()
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


    builder.toList
  }

  def response[F[_]](response: Response[F], headers: Set[CIString]): List[Attribute[_]] = {
    val builder = new ListBuffer[Attribute[_]]()

    builder += OTHttpTags.Common.status(response.status)
    response.contentLength.foreach(l => 
      builder += OTHttpTags.Common.responseContentLength(l)
    )
    builder ++=
      OTHttpTags.Headers.response(response.headers, headers)

    
    builder.toList
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