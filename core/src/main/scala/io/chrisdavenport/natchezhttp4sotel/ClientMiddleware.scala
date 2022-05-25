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
import org.typelevel.vault.Key
import org.http4s.client.middleware.Retry


object ClientMiddleware {

  val ExtraTagsKey: Key[List[(String, TraceValue)]] = Key.newKey[cats.effect.SyncIO, List[(String, TraceValue)]].unsafeRunSync()

  def trace[F[_]: natchez.Trace: MonadCancelThrow](
    ep: EntryPoint[F], // This is to escape from F Trace to Resource[F, *] timing. Which is critical
    reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
    respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
    clientSpanName: Request[F] => String = {(req: Request[F]) => s"Http Client - ${req.method}"},
    additionalRequestTags: Request[F] => Seq[(String, TraceValue)] = {(_: Request[F]) => Seq()},
    additionalResponseTags: Response[F] => Seq[(String, TraceValue)] = {(_: Response[F]) => Seq()},
  )(client: Client[F]): Client[F] = 
    Client[F]{(req: Request[F]) => 
      val base: List[(String, TraceValue)] = request(req, reqHeaders) ++ additionalRequestTags(req)
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

  def request[F[_]](request: Request[F], headers: Set[CIString]): List[(String, TraceValue)] = {
    val builder = new ListBuffer[(String, TraceValue)]()
    builder += OTHttpTags.Common.kind("client")
    builder += OTHttpTags.Common.method(request.method)
    builder += OTHttpTags.Common.url(request.uri)
    builder += OTHttpTags.Common.target(request.uri)
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
    retryCount(request.attributes).foreach{count => 
      builder += OTHttpTags.Common.retryCount(count)
    }
    builder ++= 
      OTHttpTags.Headers.request(request.headers, headers)

    builder ++= request.attributes.lookup(ExtraTagsKey).toList.flatten

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
    retryCount(response.attributes).foreach{count => 
      builder += OTHttpTags.Common.retryCount(count)
    }

    builder ++= 
      OTHttpTags.Headers.response(response.headers, headers)
    builder ++= response.attributes.lookup(ExtraTagsKey).toList.flatten
    
    builder.toList
  }

  private def retryCount(vault: org.typelevel.vault.Vault): Option[Int] = {
    // AttemptCountKey is 1,2,3,4 for the initial request,
    // since we want to do retries. We substract by 1 to get 0,1,2,3.
    vault.lookup(Retry.AttemptCountKey).map(i => i - 1)
  }

}