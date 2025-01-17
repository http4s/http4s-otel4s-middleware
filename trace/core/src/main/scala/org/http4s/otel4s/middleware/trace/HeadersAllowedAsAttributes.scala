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

package org.http4s.otel4s.middleware.trace

import org.typelevel.ci.CIString

/** Request and response headers that are allowed to be made into `Attribute`s. */
final case class HeadersAllowedAsAttributes(
    request: Set[CIString],
    response: Set[CIString],
) {

  /** @return the union of the request and response headers allowed by `this`
    *         and `that`
    */
  def union(that: HeadersAllowedAsAttributes): HeadersAllowedAsAttributes = {
    val thatReq = that.request
    val thatResp = that.response

    if (thatReq.isEmpty && thatResp.isEmpty) this
    else {
      val thisReq = this.request
      val thisResp = this.response

      if (thisReq.isEmpty && thisResp.isEmpty) that
      else if ((thisReq eq thisResp) && (thatReq eq thatResp)) {
        HeadersAllowedAsAttributes(thisReq | thatReq)
      } else {
        HeadersAllowedAsAttributes(
          request = thisReq | thatReq,
          response = thisResp | thatResp,
        )
      }
    }
  }

  /** Alias for [[`union`]] */
  def |(that: HeadersAllowedAsAttributes): HeadersAllowedAsAttributes =
    union(that)
}

object HeadersAllowedAsAttributes {

  /** @return an instance where the same set of headers is allowed for both
    *         requests and responses
    */
  def apply(allowed: Set[CIString]): HeadersAllowedAsAttributes =
    apply(allowed, allowed)

  /** The default sets of HTTP headers allowed to be turned into `Attribute`s. */
  lazy val default: HeadersAllowedAsAttributes =
    apply(defaultAllowedHttpHeaders)

  /** No headers are allowed to be made into `Attribute`s. */
  val none: HeadersAllowedAsAttributes = apply(Set.empty)

  private[this] lazy val defaultAllowedHttpHeaders: Set[CIString] = Set(
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
    "Correlation-ID",
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
    "X-Correlation-ID",
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
    "X-Real-IP",
    "X-Request-ID",
    "X-Request-Start",
    "X-Runtime",
    "X-Scheme",
    "X-SourceMap",
    "X-XSS-Protection",
  ).map(CIString(_))
}
