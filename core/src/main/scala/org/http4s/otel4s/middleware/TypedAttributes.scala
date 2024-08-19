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

import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.headers.Host
import org.http4s.headers.`User-Agent`
import org.http4s.headers.`X-Forwarded-For`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.semconv.attributes.ClientAttributes
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.attributes.UserAgentAttributes

import java.util.Locale

/** Methods for creating appropriate `Attribute`s from typed HTTP objects. */
object TypedAttributes {

  /** @return the `http.request.method` `Attribute` */
  def httpRequestMethod(method: Method): Attribute[String] =
    HttpAttributes.HttpRequestMethod(method.name)

  /** @return the `http.request.resend_count` `Attribute` */
  def httpRequestResendCount(count: Long): Attribute[Long] =
    HttpAttributes.HttpRequestResendCount(count)

  /** @return the `http.response.status_code` `Attribute` */
  def httpResponseStatusCode(status: Status): Attribute[Long] =
    HttpAttributes.HttpResponseStatusCode(status.code.toLong)

  /** @return the `network.peer.address` `Attribute` */
  def networkPeerAddress(ip: IpAddress): Attribute[String] =
    NetworkAttributes.NetworkPeerAddress(ip.toString)

  /** @return the `server.address` `Attribute` */
  def serverAddress(host: Host): Attribute[String] =
    ServerAttributes.ServerAddress(Host.headerInstance.value(host))

  /** Returns of the following `Attribute`s when their corresponding values are
    * present in the URL and not redacted by the provided [[`UriRedactor`]]:
    *
    *  - `url.full`
    *  - `url.scheme`
    *  - `url.path`
    *  - `url.query`
    *  - `url.fragment` (extremely unlikely to be present)
    */
  def url(unredacted: Uri, redactor: UriRedactor): Attributes =
    redactor.redact(unredacted).fold(Attributes.empty) { url =>
      val b = Attributes.newBuilder
      b += UrlAttributes.UrlFull(url.renderString)
      url.scheme.foreach(scheme => b += UrlAttributes.UrlScheme(scheme.value))
      if (url.path != Uri.Path.empty) b += UrlAttributes.UrlPath(url.path.renderString)
      if (url.query.nonEmpty) b += UrlAttributes.UrlQuery(url.query.renderString)
      url.fragment.foreach(b += UrlAttributes.UrlFragment(_))
      b.result()
    }

  /** @return the `user_agent.original` `Attribute` */
  def userAgentOriginal(userAgent: `User-Agent`): Attribute[String] =
    UserAgentAttributes.UserAgentOriginal(`User-Agent`.headerInstance.value(userAgent))

  /** @return the `error.type` `Attribute` */
  def errorType(cause: Throwable): Attribute[String] =
    ErrorAttributes.ErrorType(cause.getClass.getName)

  /** @return the `error.type` `Attribute` */
  def errorType(status: Status): Attribute[String] =
    ErrorAttributes.ErrorType(status.code.toString)

  /** Methods for creating appropriate `Attribute`s from typed HTTP objects
    * within an HTTP client.
    */
  object Client {

    /** @return the `server.port` `Attribute` */
    def serverPort(port: Port): Attribute[Long] =
      ServerAttributes.ServerPort(port.value.toLong)
  }

  /** Methods for creating appropriate `Attribute`s from typed HTTP objects
    * within an HTTP server.
    */
  object Server {

    /** @return the `client.address` `Attribute` */
    def clientAddress[F[_]](request: Request[F]): Option[Attribute[String]] =
      request.headers
        .get[`X-Forwarded-For`]
        .fold(request.remoteAddr)(_.values.head)
        .map(ip => ClientAttributes.ClientAddress(ip.toString))

    /** @return the `client.port` `Attribute` */
    def clientPort(port: Port): Attribute[Long] =
      ClientAttributes.ClientPort(port.value.toLong)

    /** Returns the `http.route` `Attribute`.
      *
      * Unfortunately, as this `Attribute` represents the route from the
      * application root, its value cannot be derived generically, so it is
      * private. Hopefully at some point we can develop a typed/type-safe API
      * for deriving the route from the URL or similar, and expose this method
      * at that time.
      */
    private[middleware] def httpRoute(classifiedRoute: String): Attribute[String] =
      HttpAttributes.HttpRoute(classifiedRoute)
  }

  /** Methods for creating appropriate `Attribute`s from typed HTTP headers. */
  object Headers {
    private[this] def generic(
        headers: Headers,
        allowedHeaders: Set[CIString],
        prefixKey: AttributeKey[Seq[String]],
    ): Attributes =
      headers
        .redactSensitive()
        .headers
        .groupMap(_.name)(_.value)
        .view
        .collect {
          case (name, values) if allowedHeaders.contains(name) =>
            val key =
              prefixKey
                .transformName(_ + "." + name.toString.toLowerCase(Locale.ROOT))
            Attribute(key, values)
        }
        .to(Attributes)

    /** @return `http.request.header.<lowercase name>` `Attribute`s for
      *          all headers in `allowedHeaders`
      */
    def request(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
      generic(headers, allowedHeaders, HttpAttributes.HttpRequestHeader)

    /** @return `http.response.header.<lowercase name>` `Attribute`s for
      *          all headers in `allowedHeaders`
      */
    def response(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
      generic(headers, allowedHeaders, HttpAttributes.HttpResponseHeader)

    /** The default set of headers allowed to be turned into `Attribute`s. */
    lazy val defaultAllowedHeaders: Set[CIString] = Set(
      "Accept",
      "Accept-CH",
      "Accept-Charset",
      "Accept-CH-Lifetime",
      "Accept-Encoding",
      "Accept-Language",
      "Accept-Ranges",
      "Access-Control-Allow-Credentials",
      "Access-Control-Allow-Headers",
      "Access-Control-Allow-Origin",
      "Access-Control-Expose-Methods",
      "Access-Control-Max-Age",
      "Access-Control-Request-Headers",
      "Access-Control-Request-Method",
      "Age",
      "Allow",
      "Alt-Svc",
      "B3",
      "Cache-Control",
      "Clear-Site-Data",
      "Connection",
      "Content-Disposition",
      "Content-Encoding",
      "Content-Language",
      "Content-Length",
      "Content-Location",
      "Content-Range",
      "Content-Security-Policy",
      "Content-Security-Policy-Report-Only",
      "Content-Type",
      "Cross-Origin-Embedder-Policy",
      "Cross-Origin-Opener-Policy",
      "Cross-Origin-Resource-Policy",
      "Date",
      "Deprecation",
      "Device-Memory",
      "DNT",
      "Early-Data",
      "ETag",
      "Expect",
      "Expect-CT",
      "Expires",
      "Feature-Policy",
      "Forwarded",
      "From",
      "Host",
      "If-Match",
      "If-Modified-Since",
      "If-None-Match",
      "If-Range",
      "If-Unmodified-Since",
      "Keep-Alive",
      "Large-Allocation",
      "Last-Modified",
      "Link",
      "Location",
      "Max-Forwards",
      "Origin",
      "Pragma",
      "Proxy-Authenticate",
      "Public-Key-Pins",
      "Public-Key-Pins-Report-Only",
      "Range",
      "Referer",
      "Referer-Policy",
      "Retry-After",
      "Save-Data",
      "Sec-CH-UA",
      "Sec-CH-UA-Arch",
      "Sec-CH-UA-Bitness",
      "Sec-CH-UA-Full-Version",
      "Sec-CH-UA-Full-Version-List",
      "Sec-CH-UA-Mobile",
      "Sec-CH-UA-Model",
      "Sec-CH-UA-Platform",
      "Sec-CH-UA-Platform-Version",
      "Sec-Fetch-Dest",
      "Sec-Fetch-Mode",
      "Sec-Fetch-Site",
      "Sec-Fetch-User",
      "Server",
      "Server-Timing",
      "SourceMap",
      "Strict-Transport-Security",
      "TE",
      "Timing-Allow-Origin",
      "Tk",
      "Trailer",
      "Transfer-Encoding",
      "Upgrade",
      "User-Agent",
      "Vary",
      "Via",
      "Viewport-Width",
      "Warning",
      "Width",
      "WWW-Authenticate",
      "X-B3-Sampled",
      "X-B3-SpanId",
      "X-B3-TraceId",
      "X-Content-Type-Options",
      "X-DNS-Prefetch-Control",
      "X-Download-Options",
      "X-Forwarded-For",
      "X-Forwarded-Host",
      "X-Forwarded-Port",
      "X-Forwarded-Proto",
      "X-Forwarded-Scheme",
      "X-Frame-Options",
      "X-Permitted-Cross-Domain-Policies",
      "X-Powered-By",
      "X-Real-Ip",
      "X-Request-Id",
      "X-Request-Start",
      "X-Runtime",
      "X-Scheme",
      "X-SourceMap",
      "X-XSS-Protection",
    ).map(CIString(_))
  }
}
