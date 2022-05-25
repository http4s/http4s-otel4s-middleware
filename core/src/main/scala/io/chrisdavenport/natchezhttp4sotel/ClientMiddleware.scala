package io.chrisdavenport.natchezhttp4sotel

import cats._
import cats.syntax.all._
import cats.effect.kernel._
import org.http4s._
import org.typelevel.ci.CIString
import natchez._
import scala.collection.mutable.ListBuffer
import org.http4s.headers._
import org.http4s.client._


object ClientMiddleware {

  def default[F[_]: natchez.Trace: MonadCancelThrow](ep: EntryPoint[F]): ClientMiddlewareBuilder[F] = {
    new ClientMiddlewareBuilder[F](ep, Defaults.reqHeaders, Defaults.respHeaders, Defaults.clientSpanName, Defaults.additionalRequestTags, Defaults.additionalResponseTags, Defaults.includeUrl)
  }

  object Defaults {
    val reqHeaders = OTHttpTags.Headers.defaultHeadersIncluded
    val respHeaders = OTHttpTags.Headers.defaultHeadersIncluded
    def clientSpanName[F[_]]: Request[F] => String = {(req: Request[F]) => s"Http Client - ${req.method}"}
    def additionalRequestTags[F[_]]: Request[F] => Seq[(String, TraceValue)] = {(_: Request[F]) => Seq()}
    def additionalResponseTags[F[_]]: Response[F] => Seq[(String, TraceValue)] = {(_: Response[F]) => Seq()}
    def includeUrl[F[_]]: Request[F] => Boolean = {(_: Request[F]) => true}
  }  

  final class ClientMiddlewareBuilder[F[_]: natchez.Trace: MonadCancelThrow] private[ClientMiddleware] (
    private val ep: EntryPoint[F],
    private val reqHeaders: Set[CIString],
    private val respHeaders: Set[CIString],
    private val clientSpanName: Request[F] => String,
    private val additionalRequestTags: Request[F] => Seq[(String, TraceValue)],
    private val additionalResponseTags: Response[F] => Seq[(String, TraceValue)],
    private val includeUrl: Request[F] => Boolean
  ){ self => 
    private def copy(
      ep: EntryPoint[F] = self.ep,
      reqHeaders: Set[CIString] = self.reqHeaders,
      respHeaders: Set[CIString] = self.respHeaders,
      clientSpanName: Request[F] => String = self.clientSpanName,
      additionalRequestTags: Request[F] => Seq[(String, TraceValue)] = self.additionalRequestTags,
      additionalResponseTags: Response[F] => Seq[(String, TraceValue)] = self.additionalResponseTags ,
      includeUrl: Request[F] => Boolean = self.includeUrl,
    ): ClientMiddlewareBuilder[F] = 
      new ClientMiddlewareBuilder[F](ep, reqHeaders, respHeaders, clientSpanName, additionalRequestTags, additionalResponseTags, includeUrl)

    def withRequestHeaders(reqHeaders: Set[CIString]) = copy(reqHeaders = reqHeaders)

    def withResponseHeaders(respHeaders: Set[CIString]) = copy(respHeaders = respHeaders)

    def withClientSpanName(clientSpanName: Request[F] => String) = copy(clientSpanName = clientSpanName)

    def withAdditionalRequestTags(additionalRequestTags: Request[F] => Seq[(String, TraceValue)]) =
      copy(additionalRequestTags = additionalRequestTags)

    def withAdditionalResponseTags(additionalResponseTags: Response[F] => Seq[(String, TraceValue)]) =
      copy(additionalResponseTags = additionalResponseTags)
    
    def withIncludeUrl(includeUrl: Request[F] => Boolean ) = copy(includeUrl = includeUrl)

    def build: Client[F] => Client[F] = { (client: Client[F]) => 
      Client[F]{(req: Request[F]) => 
        val base = request(req, reqHeaders, includeUrl) ++ additionalRequestTags(req)
        MonadCancelThrow[Resource[F, *]].uncancelable(poll => 
          for {
            baggage <- Resource.eval(Trace[F].kernel)
            span <- ep.continueOrElseRoot(clientSpanName(req), baggage)
            _ <- Resource.eval(span.put(base:_*))
            knl <- Resource.eval(span.kernel)
            knlHeaders = Headers(knl.toHeaders.map { case (k, v) => Header.Raw(CIString(k), v) } .toSeq)
            newReq = req.withHeaders(knlHeaders ++ req.headers)
            resp <- poll(client.run(newReq)).guaranteeCase{
              case Outcome.Succeeded(fa) => 
                Resource.eval(span.put("exit.case" -> "succeeded")) >> 
                fa.flatMap(resp => 
                  Resource.eval(
                    span.put((response(resp, respHeaders) ++ additionalResponseTags(resp)):_*)
                  )
                )
              case Outcome.Errored(e) => 
                val exitCase: (String, TraceValue) = ("exit.case" -> TraceValue.stringToTraceValue("errored"))
                val error = OTHttpTags.Errors.error(e)
                Resource.eval(
                  span.put((exitCase :: error):_*)
                )
              case Outcome.Canceled() => 
                // Canceled isn't always an error, but it generally is for http
                // TODO decide if this should add error, we do for the server side.
                Resource.eval(span.put("exit.case" -> "canceled", "canceled" -> true)) 
              
            }
            // Automatically handle client processing errors. Since this is after the response,
            // the error case will only get hit if the use block of the resulting resource happens,
            // which is the request processing stage.
            _ <- Resource.makeCase(Applicative[F].unit){
              case (_, Resource.ExitCase.Errored(e)) => span.put(OTHttpTags.Errors.error(e):_*)
              case (_, _) => Applicative[F].unit
            }
          } yield resp
        )
      }
    }
  }


  @deprecated("0.2.1", "Direct Method is Deprecated, use default with the builder instead.")
  def trace[F[_]: natchez.Trace: MonadCancelThrow](
    ep: EntryPoint[F], // This is to escape from F Trace to Resource[F, *] timing. Which is critical
    reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
    respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
    clientSpanName: Request[F] => String = {(req: Request[F]) => s"Http Client - ${req.method}"},
    additionalRequestTags: Request[F] => Seq[(String, TraceValue)] = {(_: Request[F]) => Seq()},
    additionalResponseTags: Response[F] => Seq[(String, TraceValue)] = {(_: Response[F]) => Seq()},
  )(client: Client[F]): Client[F] = 
    default(ep)
    .withRequestHeaders(reqHeaders)
    .withResponseHeaders(respHeaders)
    .withClientSpanName(clientSpanName)
    .withAdditionalRequestTags(additionalRequestTags)
    .withAdditionalResponseTags(additionalResponseTags)
    .build(client)

  private[natchezhttp4sotel] def request[F[_]](req: Request[F], headers: Set[CIString]): List[(String, TraceValue)] = {
    request(req, headers, Function.const[Boolean, Request[F]](true)(_))
  }
  def request[F[_]](request: Request[F], headers: Set[CIString], includeUrl: Request[F] => Boolean): List[(String, TraceValue)] = {
    val builder = new ListBuffer[(String, TraceValue)]()
    builder += OTHttpTags.Common.kind("client")
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

    request.remote.foreach{sa => 
      builder += 
        OTHttpTags.Common.peerIp(sa.host)
      
      builder += 
        OTHttpTags.Common.peerPort(sa.port)
      
    }
    builder ++= 
      OTHttpTags.Headers.request(request.headers, headers)
    

    builder.toList   
  }

  def response[F[_]](response: Response[F], headers: Set[CIString]): List[(String, TraceValue)] = {
    val builder = new ListBuffer[(String, TraceValue)]()

    builder += OTHttpTags.Common.status(response.status)
    response.contentLength.foreach{l => 
      builder += OTHttpTags.Common.responseContentLength(l)
    }
    // Due to negotiation. Only the response knows what protocol was selected
    builder += OTHttpTags.Common.flavor(response.httpVersion)
    builder ++= 
      OTHttpTags.Headers.response(response.headers, headers)
    
    
    builder.toList
  }



}