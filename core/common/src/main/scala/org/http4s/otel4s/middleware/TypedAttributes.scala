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
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes

import scala.annotation.unchecked.uncheckedStable

private[middleware] trait TypedAttributes {
  import TypedAttributes._

  /** @return the `error.type` `Attribute` */
  final def errorType(cause: Throwable): Attribute[String] =
    ErrorAttributes.ErrorType(cause.getClass.getName)

  /** @return the `error.type` `Attribute` */
  final def errorType(status: Status): Attribute[String] =
    ErrorAttributes.ErrorType(s"${status.code}")

  /** @return the `http.request.method` `Attribute` */
  final def httpRequestMethod(method: Method): Attribute[String] =
    if (knownMethods.contains(method)) HttpAttributes.HttpRequestMethod(method.name)
    else _httpRequestMethodOther

  /** The `http.request.method` `Attribute` with the special value `_OTHER`. */
  @uncheckedStable
  final def httpRequestMethodOther: Attribute[String] = _httpRequestMethodOther

  /** @return the `http.response.status_code` `Attribute` */
  final def httpResponseStatusCode(status: Status): Attribute[Long] =
    HttpAttributes.HttpResponseStatusCode(status.code.toLong)

  /** @return the `network.peer.address` `Attribute` */
  final def networkPeerAddress(ip: IpAddress): Attribute[String] =
    NetworkAttributes.NetworkPeerAddress(ip.toString)

  /** @return the `network.protocol.version` `Attribute` */
  final def networkProtocolVersion(version: HttpVersion): Attribute[String] = {
    val rendered = version.major match {
      case m if m <= 1 => s"$m.${version.minor}"
      case m /* if m >= 2 */ => s"$m"
    }
    NetworkAttributes.NetworkProtocolVersion(rendered)
  }
}

/** Methods for creating appropriate `Attribute`s from typed HTTP objects. */
object TypedAttributes extends TypedAttributes {
  private val knownMethods: Set[Method] = Set(
    Method.CONNECT,
    Method.DELETE,
    Method.GET,
    Method.HEAD,
    Method.OPTIONS,
    Method.PATCH,
    Method.POST,
    Method.PUT,
    Method.TRACE,
  )

  /** The `http.request.method` `Attribute` with the special value `_OTHER`. */
  private val _httpRequestMethodOther: Attribute[String] =
    HttpAttributes.HttpRequestMethod("_OTHER")

  private[middleware] val middlewareVersion: Attribute[String] =
    Attribute(
      "org.http4s.otel4s.middleware.version",
      org.http4s.otel4s.middleware.BuildInfo.version,
    )
}
