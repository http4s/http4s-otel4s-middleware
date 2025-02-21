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
package trace

import com.comcast.ip4s.Port
import org.http4s.headers.`User-Agent`
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.UserAgentAttributes

import java.util.Locale

private[middleware] trait TypedTraceAttributes extends TypedAttributes {

  /** @return the `http.request.method_original` `Attribute` */
  final def httpRequestMethodOriginal(method: Method): Attribute[String] =
    HttpAttributes.HttpRequestMethodOriginal(method.name)

  /** @return the `network.peer.port` `Attribute` */
  final def networkPeerPort(port: Port): Attribute[Long] =
    NetworkAttributes.NetworkPeerPort(port.value.toLong)

  /** @return the `user_agent.original` `Attribute` */
  final def userAgentOriginal(headers: Headers): Option[Attribute[String]] =
    headers
      .get(`User-Agent`.name)
      .map(nel => UserAgentAttributes.UserAgentOriginal(nel.head.value))

  /* header stuff here, because it's long */

  /** Methods for creating appropriate `Attribute`s from typed HTTP headers. */
  private[this] final def genericHttpHeaders(
      headers: Headers,
      redactor: HeaderRedactor,
      prefixKey: AttributeKey[Seq[String]],
  )(b: Attributes.Builder): b.type =
    b ++= headers
      .redactSensitive()
      .headers
      .groupMap(_.name)(_.value)
      .view
      .flatMap { case (name, unredactedValues) =>
        redactor
          .redactHeaderValues(name, unredactedValues)
          .map { values =>
            val key =
              prefixKey
                .transformName(_ + "." + name.toString.toLowerCase(Locale.ROOT))
            Attribute(key, values)
          }
      }

  /** Adds the `http.request.header.<lowercase name>` `Attribute`s for all
    * headers to the provided builder after redacting them.
    */
  final def httpRequestHeadersForBuilder(
      unredacted: Headers,
      redactor: HeaderRedactor,
  )(b: Attributes.Builder): b.type =
    genericHttpHeaders(unredacted, redactor, HttpAttributes.HttpRequestHeader)(b)

  /** @return `http.request.header.<lowercase name>` `Attributes` for
    *          all headers after redacting them
    */
  final def httpRequestHeaders(
      unredacted: Headers,
      redactor: HeaderRedactor,
  ): Attributes =
    httpRequestHeadersForBuilder(unredacted, redactor)(Attributes.newBuilder)
      .result()

  /** Adds the `http.response.header.<lowercase name>` `Attribute`s for all
    * headers to the provided builder after redacting them.
    */
  final def httpResponseHeadersForBuilder(
      unredacted: Headers,
      redactor: HeaderRedactor,
  )(b: Attributes.Builder): b.type =
    genericHttpHeaders(unredacted, redactor, HttpAttributes.HttpResponseHeader)(b)

  /** @return `http.response.header.<lowercase name>` `Attribute`s for
    *          all headers after redacting them
    */
  final def httpResponseHeaders(
      unredacted: Headers,
      redactor: HeaderRedactor,
  ): Attributes =
    httpResponseHeadersForBuilder(unredacted, redactor)(Attributes.newBuilder)
      .result()
}

/** Methods for creating appropriate `Attribute`s for tracing from typed HTTP
  * objects.
  */
object TypedTraceAttributes extends TypedTraceAttributes
