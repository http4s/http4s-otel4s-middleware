package org.http4s.otel4s.middleware

import cats.Applicative
import cats.effect.kernel.Outcome
import cats.effect.{Concurrent, MonadCancelThrow, Resource, SyncIO}
import cats.syntax.flatMap._
import org.http4s.client.{Client, RequestKey}
import org.http4s.{Headers, Request, Response}
import org.http4s.client.middleware.Retry
import org.http4s.headers.{Host, `User-Agent`}
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.vault.{Key, Vault}

import scala.collection.mutable.ListBuffer

object ClientMiddleware {

  def default[F[_]: Tracer: Concurrent]: ClientMiddlewareBuilder[F] = {
    new ClientMiddlewareBuilder[F](Defaults.reqHeaders, Defaults.respHeaders, Defaults.clientSpanName, Defaults.additionalRequestTags, Defaults.additionalResponseTags, Defaults.includeUrl)
  }

  object Defaults {
    val reqHeaders = OTHttpTags.Headers.defaultHeadersIncluded
    val respHeaders = OTHttpTags.Headers.defaultHeadersIncluded
    def clientSpanName[F[_]]: Request[F] => String = {(req: Request[F]) => s"Http Client - ${req.method}"}
    def additionalRequestTags[F[_]]: Request[F] => Seq[Attribute[_]] = {(_: Request[F]) => Seq()}
    def additionalResponseTags[F[_]]: Response[F] => Seq[Attribute[_]] = {(_: Response[F]) => Seq()}
    def includeUrl[F[_]]: Request[F] => Boolean = {(_: Request[F]) => true}
  }

  final class ClientMiddlewareBuilder[F[_]: Tracer: Concurrent] private[ClientMiddleware](
    private val reqHeaders: Set[CIString],
    private val respHeaders: Set[CIString],
    private val clientSpanName: Request[F] => String,
    private val additionalRequestTags: Request[F] => Seq[Attribute[_]],
    private val additionalResponseTags: Response[F] => Seq[Attribute[_]],
    private val includeUrl: Request[F] => Boolean,
  ){ self =>
    private def copy(
      reqHeaders: Set[CIString] = self.reqHeaders,
      respHeaders: Set[CIString] = self.respHeaders,
      clientSpanName: Request[F] => String = self.clientSpanName,
      additionalRequestTags: Request[F] => Seq[Attribute[_]] = self.additionalRequestTags,
      additionalResponseTags: Response[F] => Seq[Attribute[_]] = self.additionalResponseTags ,
      includeUrl: Request[F] => Boolean = self.includeUrl,
    ): ClientMiddlewareBuilder[F] =
      new ClientMiddlewareBuilder[F](reqHeaders, respHeaders, clientSpanName, additionalRequestTags, additionalResponseTags, includeUrl)

    def withRequestHeaders(reqHeaders: Set[CIString]) = copy(reqHeaders = reqHeaders)

    def withResponseHeaders(respHeaders: Set[CIString]) = copy(respHeaders = respHeaders)

    def withClientSpanName(clientSpanName: Request[F] => String) = copy(clientSpanName = clientSpanName)

    def withAdditionalRequestTags(additionalRequestTags: Request[F] => Seq[Attribute[_]]) =
      copy(additionalRequestTags = additionalRequestTags)

    def withAdditionalResponseTags(additionalResponseTags: Response[F] => Seq[Attribute[_]]) =
      copy(additionalResponseTags = additionalResponseTags)
    
    def withIncludeUrl(includeUrl: Request[F] => Boolean ) = copy(includeUrl = includeUrl)

    def build: Client[F] => Client[F] = { (client: Client[F]) =>
      Client[F] { (req: Request[F]) => // Resource[F, Response[F]]

        val base = request(req, reqHeaders, includeUrl) ++ additionalRequestTags(req)
        MonadCancelThrow[Resource[F, *]].uncancelable { poll =>

            for {
              res <- Tracer[F].span(clientSpanName(req)).resource
              span = res.span
              _ <- Resource.eval(span.addAttributes(base: _*))
              traceHeaders <- Resource.eval(Tracer[F].propagate(Headers.empty))
              newReq = req.withHeaders(traceHeaders ++ req.headers)
              resp <- poll(client.run(newReq)).guaranteeCase {
                case Outcome.Succeeded(fa) =>
                  Resource.eval(span.addAttribute(Attribute("exit.case", "succeeded"))) >>
                  fa.flatMap(resp =>
                    Resource.eval(
                      span.addAttributes(response(resp, respHeaders) ++ additionalResponseTags(resp): _*)
                    )
                  )
                case Outcome.Errored(e) =>
                  Resource.eval(
                    span.recordException(e) >>
                      span.addAttribute(Attribute("exit.case", "errored"))
                  )
                case Outcome.Canceled() =>
                  // Canceled isn't always an error, but it generally is for http
                  // TODO decide if this should add error, we do for the server side.
                  Resource.eval(span.addAttributes(Attribute("exit.case", "canceled"), Attribute("canceled", true)))

              }
              // Automatically handle client processing errors. Since this is after the response,
              // the error case will only get hit if the use block of the resulting resource happens,
              // which is the request processing stage.
              _ <- Resource.makeCase(Applicative[F].unit){
                case (_, Resource.ExitCase.Errored(e)) => span.recordException(e)
                case (_, _) => Applicative[F].unit
              }
            } yield resp
        }
      }
    }
  }

  val ExtraTagsKey: Key[List[Attribute[_]]] = Key.newKey[SyncIO, List[Attribute[_]]].unsafeRunSync()

  private[middleware] def request[F[_]](req: Request[F], headers: Set[CIString]): List[Attribute[_]] = {
    request(req, headers, Function.const[Boolean, Request[F]](true)(_))
  }
  def request[F[_]](request: Request[F], headers: Set[CIString], includeUrl: Request[F] => Boolean): List[Attribute[_]] = {
    val builder = new ListBuffer[Attribute[_]]()
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
    retryCount(request.attributes).foreach{count => 
      builder += OTHttpTags.Common.retryCount(count)
    }
    builder ++= 
      OTHttpTags.Headers.request(request.headers, headers)

    builder ++= request.attributes.lookup(ExtraTagsKey).toList.flatten

    builder.toList
  }



  def response[F[_]](response: Response[F], headers: Set[CIString]): List[Attribute[_]] = {
    val builder = new ListBuffer[Attribute[_]]()

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

  private def retryCount(vault: Vault): Option[Int] = {
    // AttemptCountKey is 1,2,3,4 for the initial request,
    // since we want to do retries. We substract by 1 to get 0,1,2,3.
    vault.lookup(Retry.AttemptCountKey).map(i => i - 1)
  }

}
