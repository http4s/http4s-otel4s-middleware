package io.chrisdavenport.natchezhttp4sotel

import cats._
import cats.syntax.all._
import cats.effect.kernel._
import cats.effect.syntax.all._
import org.http4s._
import org.typelevel.ci.CIString
import natchez._
import scala.collection.mutable.ListBuffer
import org.http4s.headers._
import org.http4s.client._
import io.chrisdavenport.fiberlocal._
import cats.data.OptionT
import cats.arrow.FunctionK

object ServerMiddleware {

  // Recommended to get best tracing
  def httpApp[F[_]: MonadCancelThrow: GenFiberLocal](
    ep: EntryPoint[F], 
    isKernelHeader: CIString => Boolean = name => !ExcludedHeaders.contains(name),
    reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
    respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded
  )(f: Trace[F] => HttpApp[F]): HttpApp[F] = 
    MakeSureYouKnowWhatYouAreDoing.tracedF(ep, FunctionK.id[F], isKernelHeader, reqHeaders, respHeaders)(f.andThen(_.pure[F]))

  def httpRoutes[F[_]: MonadCancelThrow: GenFiberLocal](
    ep: EntryPoint[F], 
    isKernelHeader: CIString => Boolean = name => !ExcludedHeaders.contains(name),
    reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
    respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded
  )(f: Trace[F] => HttpRoutes[F]): HttpRoutes[F] = 
    MakeSureYouKnowWhatYouAreDoing.tracedF(ep, OptionT.liftK, isKernelHeader, reqHeaders, respHeaders)(f.andThen(_.pure[OptionT[F, *]]))

  object MakeSureYouKnowWhatYouAreDoing {
    // This effect to generate routes will run on every request.
    // This is often undesired and can generate a lot of wasted state if used
    // incorrectly. Should never be used to instantiate global state across request,
    // the effect is scoped to a single request in. But as we see for the fiberlocal
    // with this can be pretty useful when its what you need.
    def tracedF[F[_]: MonadCancelThrow: GenFiberLocal, G[_]: MonadCancelThrow](
      ep: EntryPoint[F],
      fk: F ~> G,
      isKernelHeader: CIString => Boolean = name => !ExcludedHeaders.contains(name),
      reqHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
      respHeaders: Set[CIString] = OTHttpTags.Headers.defaultHeadersIncluded,
      routeClassifier: Request[F] => Option[String] = {(_: Request[F]) => None},
      serverSpanName: Request[F] => String = {(req: Request[F]) => s"Http Server - ${req.method}"}
    )(
      f: Trace[F] => G[Http[G, F]]
    ): Http[G, F] = cats.data.Kleisli{(req: Request[F]) => 
      val kernelHeaders = req.headers.headers
        .collect {
          case header if isKernelHeader(header.name) => header.name.toString -> header.value
        }
        .toMap

      val kernel = Kernel(kernelHeaders)

      MonadCancelThrow[G].uncancelable(poll => 
        ep.continueOrElseRoot(serverSpanName(req), kernel).mapK(fk).use{span => 
          val init = request(req, reqHeaders, routeClassifier)
          fk(span.put(init:_*)) >>
          fk(GenFiberLocal[F].local(span)).map(fromFiberLocal(_))
            .flatMap( trace =>
              poll(f(trace).flatMap(_.run(req))).guaranteeCase{
                case Outcome.Succeeded(fa) => 
                  fk(span.put("exit.case" -> "succeeded")) >>
                  fa.flatMap{resp => 
                    val out = response(resp, respHeaders)
                    fk(span.put(out:_*))
                  }
                case Outcome.Errored(e) => 
                  fk(span.put("exit.case" -> "errored")) >>
                  fk(span.put(OTHttpTags.Errors.error(e):_*))
                case Outcome.Canceled() => 
                  fk(span.put(
                    "exit.case" -> "canceled",
                    "canceled" -> true,
                    "error" -> true // A cancelled http is an error for the server. The connection got cut for some reason.
                  ))
              }
            )
        }
      )
    }
  }

  def request[F[_]](request: Request[F], headers: Set[CIString], routeClassifier: Request[F] => Option[String]): List[(String, TraceValue)] = {
    val builder = new ListBuffer[(String, TraceValue)]()
    builder += OTHttpTags.Common.kind("server")
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

  def response[F[_]](response: Response[F], headers: Set[CIString]): List[(String, TraceValue)] = {
    val builder = new ListBuffer[(String, TraceValue)]()

    builder += OTHttpTags.Common.status(response.status)
    response.contentLength.foreach(l => 
      builder += OTHttpTags.Common.responseContentLength(l)
    )
    builder ++= 
      OTHttpTags.Headers.response(response.headers, headers)
    
    
    builder.toList
  }

  private def fromFiberLocal[F[_]: MonadCancelThrow](local: FiberLocal[F, Span[F]]): natchez.Trace[F] = 
    new natchez.Trace[F] {
      def put(fields: (String, TraceValue)*): F[Unit] =
        local.get.flatMap(_.put(fields: _*))

      def kernel: F[Kernel] =
        local.get.flatMap(_.kernel)

      def span[A](name: String)(k: F[A]): F[A] =
        local.get.flatMap { parent =>
          parent.span(name).flatMap { child =>
            Resource.make(local.set(child))(_ => local.set(parent))
          } .use { _ => k }
        }

      def traceId: F[Option[String]] =
        local.get.flatMap(_.traceId)

      def traceUri =
        local.get.flatMap(_.traceUri)
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