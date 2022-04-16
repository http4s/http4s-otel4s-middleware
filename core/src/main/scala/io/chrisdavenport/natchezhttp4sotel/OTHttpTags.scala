package io.chrisdavenport.natchezhttp4sotel

import org.http4s._
import natchez._
import org.http4s.headers._
import com.comcast.ip4s._
import org.typelevel.ci.CIString
import cats.syntax.all._
import io.chrisdavenport.natchezhttp4sotel.helpers.printStackTrace
// This follows the documents here
// https://github.com/open-telemetry/opentelemetry-specification/blob/a50def370ef444029a12ea637769229768daeaf8/specification/trace/semantic_conventions/http.md
// We can update both the link and the tags as standards develop out of experimental

object OTHttpTags {
  object Common {
    def method(m: Method): (String, TraceValue) = ("http.method", m.name)
    def url(url: Uri): (String, TraceValue) = ("http.url", url.renderString)
    def target(url: Uri): (String, TraceValue) = ("http.target", url.copy(scheme = None, authority = None).renderString)
    def host(host: org.http4s.headers.Host): (String, TraceValue) = ("http.host", org.http4s.headers.Host.headerInstance.value(host))
    def scheme(scheme: Uri.Scheme): (String, TraceValue) = ("http.scheme", scheme.value)
    
    def status(status: Status): (String, TraceValue) = ("http.status_code", status.code)
    // Need to check both request and response in case negotiation happens
    def flavor(httpVersion: HttpVersion): (String, TraceValue) = ("http.flavor", httpVersion.major.toString() ++ "." ++ httpVersion.minor.toString())
    def userAgent(userAgent: `User-Agent`): (String, TraceValue) = ("http.user_agent", `User-Agent`.headerInstance.value(userAgent))
    def requestContentLength(cl: Long): (String, TraceValue) = ("http.request_content_length", cl.toInt)
    def responseContentLength(cl: Long): (String, TraceValue) = ("http.response_content_length", cl.toInt)
    def retryCount(i: Int): (String, TraceValue) = ("http.retry_count", i)
    def peerIp(ip: IpAddress): (String, TraceValue) = ("net.peer.ip", ip.toString()) // TODO: Check that this is the right way
    def peerPort(port: Port): (String, TraceValue) = ("net.peer.port", port.value)
  }

  object Client {
    def peerName(uri: Uri): Option[(String, TraceValue)] = uri.host.map(h => "net.peer.name" -> h.value)
  }

  object Server {
    def serverName(s: String): (String, TraceValue) = ("http.server_name", s)
    // The route template. Since http4s uses unapplies by default this is non-trivial.
    // Accept-List for segements are better to get coherent routes, but block-list segments
    // take much less effort while allowing unexpected variables. We will provide options
    // for this
    def route(s: String): (String, TraceValue) = ("http.route", s)
    // This is either the net ip, OR the x-forwarded for depending on what is available.
    def clientIp(ip: IpAddress): (String, TraceValue) = ("http.client_ip", ip.toString())
  }


  object Headers {

    // TODO: Otel here is a []string, not a single string. I have chosen this for simplicity, but we can do better.
    // s is a whitelisted set of Headers to include, any headers not there will not appear.
    private def generic(headers: Headers, s: Set[CIString], messageType: String): List[(String, TraceValue)] = {
      headers.headers.filter(h => s.contains(h.name))
        .groupBy(r => (r.name))
        .toList
        .map{
          case (name, list) => ("http." ++ messageType ++ ".header.string." ++ name.toString.toLowerCase.replace("-", "_"), list.map(_.value).mkString(", "))
        }.map{ case (name, s) => name -> TraceValue.stringToTraceValue(s)} // We add a string as a prefix, because the otel standard is an array so 
          // that way we don't have bad values in the canonical space we'll want to use when we can.
    }

    def request(headers: Headers, s: Set[CIString]): List[(String, TraceValue)] = 
      generic(headers, s, "request")
    def response(headers: Headers, s: Set[CIString]): List[(String, TraceValue)] =
      generic(headers, s, "response")

    lazy val defaultHeadersIncluded = Set(
      CIString("x-request-id"),
      Forwarded.headerInstance.name,
      `X-Forwarded-For`.headerInstance.name,
      `Content-Type`.headerInstance.name,
      `Content-Encoding`.headerInstance.name,
      Connection.headerInstance.name,
      `Transfer-Encoding`.headerInstance.name,
      Upgrade.headerInstance.name,
      `Keep-Alive`.headerInstance.name,
      `Cache-Control`.headerInstance.name,
      Age.headerInstance.name,
      Expires.headerInstance.name,
      CIString("pragma"),
      `Last-Modified`.headerInstance.name,
      ETag.headerInstance.name,
      `If-Match`.headerInstance.name,
      `If-None-Match`.headerInstance.name,
      `If-Modified-Since`.headerInstance.name,
      `If-Unmodified-Since`.headerInstance.name,
      CIString("Vary"),
      CIString("Via"),
      Accept.headerInstance.name,
      `Accept-Encoding`.headerInstance.name,
      `Accept-Language`.headerInstance.name,
      Origin.headerInstance.name,
      Location.headerInstance.name,
      Referer.headerInstance.name,
      Allow.headerInstance.name,
      org.http4s.headers.Server.headerInstance.name,
      CIString("Expect"),
      CIString("Trailer"),
      CIString("TE"),
      CIString("Sec-CH-UA"),
      CIString("Sec-CH-UA-Arch"),
      CIString("Sec-CH-UA-Bitness"),
      CIString("Sec-CH-UA-Full-Version-List"),
      CIString("Sec-CH-UA-Full-Version"),
      CIString("Sec-CH-UA-Mobile"),
      CIString("Sec-CH-UA-Model"),
      CIString("Sec-CH-UA-Platform"),
      CIString("Sec-CH-UA-Platform-Version")
    )
  }

  object Errors {
    def error(e: Throwable): List[(String, TraceValue)] = {
      val error = ("error", TraceValue.boolToTraceValue(true)).some
      val message: Option[(String, TraceValue)] = Option(e.getMessage()).map(m => "error.message" -> m)
      val className: Option[(String, TraceValue)] = Option(e.getClass()).flatMap(c => Option(c.getName())).map(c => "error.class" -> c)
      val stacktrace = ("error.stacktrace" -> TraceValue.stringToTraceValue(printStackTrace(e))).some
      List(error, message, className, stacktrace).flatten // List[Option[A]] => List[A] using internal speedery
    }
  }

}