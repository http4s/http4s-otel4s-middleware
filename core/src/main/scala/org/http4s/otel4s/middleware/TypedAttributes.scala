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
import org.http4s.Query
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
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.attributes.UserAgentAttributes

import java.util.Locale

/** Methods for creating appropriate `Attribute`s from typed HTTP objects. */
object TypedAttributes {
  def httpRequestMethod(method: Method): Attribute[String] =
    HttpAttributes.HttpRequestMethod(method.name)
  def httpRequestResendCount(count: Long): Attribute[Long] =
    HttpAttributes.HttpRequestResendCount(count)
  def httpResponseStatusCode(status: Status): Attribute[Long] =
    HttpAttributes.HttpResponseStatusCode(status.code.toLong)
  def networkPeerAddress(ip: IpAddress): Attribute[String] =
    NetworkAttributes.NetworkPeerAddress(ip.toString)
  def serverAddress(host: Host): Attribute[String] =
    ServerAttributes.ServerAddress(Host.headerInstance.value(host))
  def urlFull(url: Uri): Attribute[String] =
    UrlAttributes.UrlFull(url.renderString)
  def urlPath(path: Uri.Path): Attribute[String] =
    UrlAttributes.UrlPath(path.renderString)
  def urlQuery(query: Query): Attribute[String] =
    UrlAttributes.UrlQuery(query.renderString)
  def urlScheme(scheme: Uri.Scheme): Attribute[String] =
    UrlAttributes.UrlScheme(scheme.value)
  def userAgentOriginal(userAgent: `User-Agent`): Attribute[String] =
    UserAgentAttributes.UserAgentOriginal(`User-Agent`.headerInstance.value(userAgent))

  /** Methods for creating appropriate `Attribute`s from typed HTTP objects
    * within an HTTP client.
    */
  object Client {
    def serverPort(port: Port): Attribute[Long] =
      ServerAttributes.ServerPort(port.value.toLong)
  }

  /** Methods for creating appropriate `Attribute`s from typed HTTP objects
    * within an HTTP server.
    */
  object Server {
    def clientAddress[F[_]](request: Request[F]): Option[Attribute[String]] =
      request.headers
        .get[`X-Forwarded-For`]
        .fold(request.remoteAddr)(_.values.head)
        .map(ip => ClientAttributes.ClientAddress(ip.toString))
    def clientPort(port: Port): Attribute[Long] =
      ClientAttributes.ClientPort(port.value.toLong)

    private[middleware] def httpRoute(classifiedRoute: String): Attribute[String] =
      HttpAttributes.HttpRoute(classifiedRoute)
  }

  /** Methods for creating appropriate `Attribute`s from typed HTTP headers. */
  object Headers {
    private[this] def unsafeGeneric(
        redactedHeaders: Headers,
        allowedHeaders: Set[CIString],
        prefixKey: AttributeKey[Seq[String]],
    ): Attributes =
      redactedHeaders.headers
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

    private[this] def generic(
        headers: Headers,
        allowedHeaders: Set[CIString],
        prefixKey: AttributeKey[Seq[String]],
    ): Attributes =
      unsafeGeneric(headers.redactSensitive(), allowedHeaders, prefixKey)

    def request(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
      generic(headers, allowedHeaders, HttpAttributes.HttpRequestHeader)
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
