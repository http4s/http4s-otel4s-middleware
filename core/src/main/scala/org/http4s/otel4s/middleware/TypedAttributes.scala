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

package org.http4s
package otel4s.middleware

import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port
import org.http4s.headers.`User-Agent`
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UserAgentAttributes

import java.util.Locale

/** Methods for creating appropriate `Attribute`s from typed HTTP objects. */
object TypedAttributes {
  private[this] lazy val knownMethods: Set[Method] = Method.all.toSet
  private[middleware] val middlewareVersion: Attribute[String] =
    Attribute(
      "org.http4s.otel4s.middleware.version",
      org.http4s.otel4s.middleware.BuildInfo.version,
    )

  /** The http.request.method `Attribute` with the special value _OTHER */
  val httpRequestMethodOther: Attribute[String] =
    HttpAttributes.HttpRequestMethod("_OTHER")

  /** @return the `error.type` `Attribute` */
  def errorType(cause: Throwable): Attribute[String] =
    ErrorAttributes.ErrorType(cause.getClass.getName)

  /** @return the `error.type` `Attribute` */
  def errorType(status: Status): Attribute[String] =
    ErrorAttributes.ErrorType(s"${status.code}")

  /** @return the `http.request.method` `Attribute` */
  def httpRequestMethod(method: Method): Attribute[String] =
    if (knownMethods.contains(method)) HttpAttributes.HttpRequestMethod(method.name)
    else httpRequestMethodOther

  /** @return the `http.request.method_original` `Attribute` */
  def httpRequestMethodOriginal(method: Method): Attribute[String] =
    HttpAttributes.HttpRequestMethodOriginal(method.name)

  /** @return the `http.response.status_code` `Attribute` */
  def httpResponseStatusCode(status: Status): Attribute[Long] =
    HttpAttributes.HttpResponseStatusCode(status.code.toLong)

  /** @return the `network.peer.address` `Attribute` */
  def networkPeerAddress(ip: IpAddress): Attribute[String] =
    NetworkAttributes.NetworkPeerAddress(ip.toString)

  /** @return the `network.peer.port` `Attribute` */
  def networkPeerPort(port: Port): Attribute[Long] =
    NetworkAttributes.NetworkPeerPort(port.value.toLong)

  /** @return the `network.protocol.version` `Attribute` */
  def networkProtocolVersion(version: HttpVersion): Attribute[String] = {
    val rendered = version.major match {
      case m if m <= 1 => s"$m.${version.minor}"
      case m /* if m >= 2 */ => s"$m"
    }
    NetworkAttributes.NetworkProtocolVersion(rendered)
  }

  /** @return the `user_agent.original` `Attribute` */
  def userAgentOriginal(headers: Headers): Option[Attribute[String]] =
    headers
      .get(`User-Agent`.name)
      .map(nel => UserAgentAttributes.UserAgentOriginal(nel.head.value))

  /* header stuff here, because it's long */

  /** Methods for creating appropriate `Attribute`s from typed HTTP headers. */
  private[this] def genericHttpHeaders(
      headers: Headers,
      allowedHeaders: Set[CIString],
      prefixKey: AttributeKey[Seq[String]],
  )(b: Attributes.Builder): b.type =
    b ++= headers
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

  /** Adds the `http.request.header.<lowercase name>` `Attribute`s for all
    * headers in `allowedHeaders` to the provided builder.
    */
  def httpRequestHeadersForBuilder(headers: Headers, allowedHeaders: Set[CIString])(
      b: Attributes.Builder
  ): b.type =
    if (allowedHeaders.isEmpty) b
    else genericHttpHeaders(headers, allowedHeaders, HttpAttributes.HttpRequestHeader)(b)

  /** @return `http.request.header.<lowercase name>` `Attributes` for
    *          all headers in `allowedHeaders`
    */
  def httpRequestHeaders(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
    if (allowedHeaders.isEmpty) Attributes.empty
    else httpRequestHeadersForBuilder(headers, allowedHeaders)(Attributes.newBuilder).result()

  /** Adds the `http.response.header.<lowercase name>` `Attribute`s for all
    * headers in `allowedHeaders` to the provided builder.
    */
  def httpResponseHeadersForBuilder(headers: Headers, allowedHeaders: Set[CIString])(
      b: Attributes.Builder
  ): b.type =
    if (allowedHeaders.isEmpty) b
    else genericHttpHeaders(headers, allowedHeaders, HttpAttributes.HttpResponseHeader)(b)

  /** @return `http.response.header.<lowercase name>` `Attribute`s for
    *          all headers in `allowedHeaders`
    */
  def httpResponseHeaders(headers: Headers, allowedHeaders: Set[CIString]): Attributes =
    if (allowedHeaders.isEmpty) Attributes.empty
    else httpResponseHeadersForBuilder(headers, allowedHeaders)(Attributes.newBuilder).result()
}
