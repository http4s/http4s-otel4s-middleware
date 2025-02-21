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
package server

import com.comcast.ip4s.IpAddress
import org.http4s.headers.Forwarded
import org.http4s.headers.`X-Forwarded-For`
import org.http4s.otel4s.middleware.server.TypedServerAttributes
import org.http4s.otel4s.middleware.server.findFirstInForwarded
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.semconv.attributes.ClientAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes

/** Methods for creating appropriate `Attribute`s for tracing from typed HTTP
  * objects within an HTTP server.
  */
object TypedServerTraceAttributes extends TypedServerAttributes with TypedTraceAttributes {
  private[this] def addressFromNodeName(nodeName: Forwarded.Node.Name): Option[IpAddress] =
    nodeName match {
      case Forwarded.Node.Name.Ipv4(address) => Some(address)
      case Forwarded.Node.Name.Ipv6(address) => Some(address)
      case _ => None
    }

  private[this] def clientAddress(address: IpAddress): Attribute[String] =
    ClientAttributes.ClientAddress(address.toString)

  private[this] def clientAddressAndMaybePortForBuilder[F[_]](
      request: Request[F],
      forwarded: Option[Forwarded],
      includePort: Boolean,
  )(b: Attributes.Builder): b.type =
    forwarded
      .flatMap(findFirstInForwarded(_, _.maybeFor))
      .map[b.type] { node =>
        if (includePort) {
          b ++= node.nodePort
            .collect { case Forwarded.Node.Port.Numeric(port) =>
              ClientAttributes.ClientPort(port.toLong)
            }
        }
        b ++= addressFromNodeName(node.nodeName).map(clientAddress)
      }
      .orElse[b.type] {
        request.headers
          .get[`X-Forwarded-For`]
          .map(b ++= _.values.head.map(clientAddress))
      }
      .getOrElse {
        if (includePort) {
          b ++= request.remotePort
            .map(port => ClientAttributes.ClientPort(port.value.toLong))
        }
        b ++= request.remoteAddr.map(clientAddress)
      }

  /** @param request the client's request
    * @param forwarded the `Forwarded` header, if present in the request.
    *                  Because it is used in the creation of several
    *                  `Attribute`s, it is parsed and provided separately.
    *                  If not provided in this parameter, it will not be read
    *                  from `request`.
    * @return the `client.address` `Attribute`
    */
  def clientAddress[F[_]](
      request: Request[F],
      forwarded: Option[Forwarded],
  ): Option[Attribute[String]] =
    clientAddressAndMaybePortForBuilder(
      request,
      forwarded,
      includePort = false,
    )(Attributes.newBuilder)
      .result()
      .get(ClientAttributes.ClientAddress)

  /** Adds the `client.address` and `client.port` `Attribute`s to the provided
    * builder.
    *
    * @param request the client's request
    * @param forwarded the `Forwarded` header, if present in the request.
    *                  Because it is used in the creation of several
    *                  `Attribute`s, it is parsed and provided separately.
    *                  If not provided in this parameter, it will not be read
    *                  from `request`.
    * @param b the builder to which to append `Attribute`s
    */
  def clientAddressAndPortForBuilder[F[_]](
      request: Request[F],
      forwarded: Option[Forwarded],
  )(b: Attributes.Builder): b.type =
    clientAddressAndMaybePortForBuilder(request, forwarded, includePort = true)(b)

  /** @param request the client's request
    * @param forwarded the `Forwarded` header, if present in the request.
    *                  Because it is used in the creation of several
    *                  `Attribute`s, it is parsed and provided separately.
    *                  If not provided in this parameter, it will not be read
    *                  from `request`.
    * @return the `client.address` and `client.port` `Attributes`
    */
  def clientAddressAndPort[F[_]](
      request: Request[F],
      forwarded: Option[Forwarded],
  ): Attributes =
    clientAddressAndPortForBuilder(request, forwarded)(Attributes.newBuilder).result()

  /** @return the `url.path` `Attribute` */
  def urlPath(unredacted: Uri.Path, redactor: redact.PathRedactor): Option[Attribute[String]] = {
    val path = redactor.redactPath(unredacted)
    Option.unless(path == Uri.Path.empty)(UrlAttributes.UrlPath(path.renderString))
  }

  /** @return the `url.query` `Attribute` */
  def urlQuery(unredacted: Query, redactor: redact.QueryRedactor): Option[Attribute[String]] = {
    val query = redactor.redactQuery(unredacted)
    Option.unless(query.isEmpty)(UrlAttributes.UrlQuery(query.renderString))
  }
}
