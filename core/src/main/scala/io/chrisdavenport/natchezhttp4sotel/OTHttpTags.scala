package io.chrisdavenport.natchezhttp4sotel

import org.http4s._
import org.typelevel.otel4s.Attribute
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
    def kind(kind: String): Attribute[String] = Attribute("span.kind" , kind)
    def method(m: Method): Attribute[String] = Attribute("http.method" , m.name)
    def url(url: Uri): Attribute[String] = Attribute("http.url" , url.renderString)
    def target(url: Uri): Attribute[String] = Attribute("http.target" , url.copy(scheme = None, authority = None).renderString)
    def host(host: org.http4s.headers.Host): Attribute[String] = Attribute("http.host" , org.http4s.headers.Host.headerInstance.value(host))
    def scheme(scheme: Uri.Scheme): Attribute[String] = Attribute("http.scheme" , scheme.value)
    
    def status(status: Status): Attribute[Long] = Attribute("http.status_code" , status.code.toLong)
    // Need to check both request and response in case negotiation happens
    def flavor(httpVersion: HttpVersion): Attribute[String] = Attribute("http.flavor" , httpVersion.major.toString() ++ "." ++ httpVersion.minor.toString())
    def userAgent(userAgent: `User-Agent`): Attribute[String] = Attribute("http.user_agent" , `User-Agent`.headerInstance.value(userAgent))
    def requestContentLength(cl: Long): Attribute[Long] = Attribute("http.request_content_length" , cl)
    def responseContentLength(cl: Long): Attribute[Long] = Attribute("http.response_content_length" , cl)
    def retryCount(i: Int): Attribute[Long] = Attribute("http.retry_count" , i.toLong)
    def peerIp(ip: IpAddress): Attribute[String] = Attribute("net.peer.ip" , ip.toString()) // TODO: Check that this is the right way
    def peerPort(port: Port): Attribute[Long] = Attribute("net.peer.port" , port.value)
  }

  object Client {
    def peerName(uri: Uri): Option[Attribute[String]] = uri.host.map(h => Attribute("net.peer.name" , h.value))
  }

  object Server {
    def serverName(s: String): Attribute[String] = Attribute("http.server_name" , s)
    // The route template. Since http4s uses unapplies by default this is non-trivial.
    // Accept-List for segements are better to get coherent routes, but block-list segments
    // take much less effort while allowing unexpected variables. We will provide options
    // for this
    def route(s: String): Attribute[String] = Attribute("http.route" , s)
    // This is either the net ip, OR the x-forwarded for depending on what is available.
    def clientIp(ip: IpAddress): Attribute[String] = Attribute("http.client_ip" , ip.toString())
  }


  object Headers {

    // TODO: Otel here is a []string, not a single string. I have chosen this for simplicity, but we can do better.
    // s is a whitelisted set of Headers to include, any headers not there will not appear.
    private def generic(headers: Headers, s: Set[CIString], messageType: String): List[Attribute[_]] = {
      headers.headers.filter(h => s.contains(h.name))
        .groupBy(r => (r.name))
        .toList
        .map{
          case (name, list) =>
            Attribute("http." ++ messageType ++ ".header." ++ name.toString.toLowerCase.replace("-", "_"), list.map(_.value))
        }
    }

    def request(headers: Headers, s: Set[CIString]): List[Attribute[_]] =
      generic(headers, s, "request")
    def response(headers: Headers, s: Set[CIString]): List[Attribute[_]] =
      generic(headers, s, "response")

    lazy val defaultHeadersIncluded = Set(
      "WWW-Authenticate", "Proxy-Authenticate",
      "Age", "Cache-Control", "Clear-Site-Data", "Expires", "Pragma", "Warning",
      "Accept-CH", "Accept-CH-Liftetime", "Early-Data", "Device-Memory", "Save-Data", "Viewport-Width", "Width",
      "Last-Modified", "ETag", "If-Match", "If-None-Match", "If-Modified-Since", "If-Unmodified-Since", "Vary",
      "Connection", "Keep-Alive",
      "Accept", "Accept-Charset", "Accept-Encoding", "Accept-Language",
      "Expect", "Max-Forwards",
      
      "Access-Control-Allow-Origin",
      "Access-Control-Allow-Credentials",
      "Access-Control-Allow-Headers",
      "Access-Control-Expose-Methods",
      "Access-Control-Max-Age",
      "Access-Control-Request-Headers",
      "Access-Control-Request-Method",
      "Origin",
      "Timing-Allow-Origin",

      "DNT", "Tk",
      "Content-Disposition",
      "Content-Length", "Content-Type", "Content-Encoding", "Content-Language", "Content-Location",
      "Forwarded", "X-Forwarded-For", "X-Forwarded-Host", "X-Forwarded-Proto", "Via",
      "Location",
      "From", "Host", "Referer", "Referer-Policy", "User-Agent",
      "Allow", "Server",
      "Accept-Ranges", "Range", "If-Range", "Content-Range",

      "Cross-Origin-Embedder-Policy",
      "Cross-Origin-Opener-Policy",
      "Cross-Origin-Resource-Policy",
      "Content-Security-Policy",
      "Content-Security-Policy-Report-Only",
      "Expect-CT",
      "Feature-Policy",
      "Strict-Transport-Security",
      "X-Content-Type-Options",
      "X-Download-Options",
      "X-Frame-Options",
      "X-Permitted-Cross-Domain-Policies",
      "X-Powered-By",
      "X-XSS-Protection",

      "Public-Key-Pins", "Public-Key-Pins-Report-Only",
      "Sec-Fetch-Site", "Sec-Fetch-Mode", "Sec-Fetch-User", "Sec-Fetch-Dest",

      "Transfer-Encoding", "TE", "Trailer",

      "Alt-Svc",
      "Date",
      "Large-Allocation",
      "Link",
      "Retry-After",
      "Server-Timing",
      "SourceMap",
      "X-SourceMap",
      "Upgrade",
      "X-DNS-Prefetch-Control",
      "X-Request-Id",

      "Sec-CH-UA",
      "Sec-CH-UA-Arch",
      "Sec-CH-UA-Bitness",
      "Sec-CH-UA-Full-Version-List",
      "Sec-CH-UA-Full-Version",
      "Sec-CH-UA-Mobile",
      "Sec-CH-UA-Model",
      "Sec-CH-UA-Platform",
      "Sec-CH-UA-Platform-Version"
    ).map(CIString(_))
  }

  // https://github.com/open-telemetry/opentelemetry-specification/blob/a50def370ef444029a12ea637769229768daeaf8/specification/trace/semantic_conventions/exceptions.md
  object Errors {
    def error(e: Throwable): List[Attribute[_]] = {
      val error: Option[Attribute[Boolean]] = Attribute("error" ,true).some
      val message: Option[Attribute[String]] = Option(e.getMessage()).map(m => Attribute("exception.message" , m))
      val className: Option[Attribute[String]] = Option(e.getClass()).flatMap(c => Option(c.getName())).map(c => Attribute("exception.type" , c))
      val stacktrace: Option[Attribute[List[String]]] = Attribute("exception.stacktrace" , helpers.listStackTrace(e)).some
      List(error, message, className, stacktrace).flatten // List[Option[A]] => List[A] using internal speedery
    }
  }

}