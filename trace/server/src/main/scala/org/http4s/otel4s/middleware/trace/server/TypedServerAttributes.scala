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
import org.http4s.headers.Host
import org.http4s.headers.`X-Forwarded-For`
import org.http4s.headers.`X-Forwarded-Host`
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.semconv.attributes.ClientAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes

/** Methods for creating appropriate `Attribute`s from typed HTTP objects
  * within an HTTP server.
  */
object TypedServerAttributes {
  private[this] def addressFromNodeName(nodeName: Forwarded.Node.Name): Option[IpAddress] =
    nodeName match {
      case Forwarded.Node.Name.Ipv4(address) => Some(address)
      case Forwarded.Node.Name.Ipv6(address) => Some(address)
      case _ => None
    }

  private[this] def clientAddress(address: IpAddress): Attribute[String] =
    ClientAttributes.ClientAddress(address.toString)

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
    forwarded
      .flatMap(findFirstInForwarded(_, _.maybeFor))
      .map[b.type] { node =>
        b ++= addressFromNodeName(node.nodeName).map(clientAddress)
        b ++= node.nodePort
          .collect { case Forwarded.Node.Port.Numeric(port) =>
            ClientAttributes.ClientPort(port.toLong)
          }
      }
      .orElse[b.type] {
        request.headers
          .get[`X-Forwarded-For`]
          .map(b ++= _.values.head.map(clientAddress))
      }
      .getOrElse {
        b ++= request.remoteAddr.map(clientAddress)
        b ++= request.remotePort
          .map(port => ClientAttributes.ClientPort(port.value.toLong))
      }

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

  /** @return the `http.route` `Attribute` */
  def httpRoute(request: RequestPrelude, classifier: RouteClassifier): Option[Attribute[String]] =
    classifier.classify(request).map(HttpAttributes.HttpRoute.apply)

  /** @return the `http.route` `Attribute` */
  def httpRoute[F[_]](request: Request[F], classifier: RouteClassifier): Option[Attribute[String]] =
    httpRoute(request.requestPrelude, classifier)

  /** Adds the `server.address` and `server.port` `Attribute`s to the provided
    * builder.
    *
    * @param request the client's request
    * @param forwarded the `Forwarded` header, if present in the request.
    *                  Because it is used in the creation of several
    *                  `Attribute`s, it is parsed and provided separately.
    *                  If not provided in this parameter, it will not be read
    *                  from `request`.
    * @param scheme the original scheme of the request
    * @param b the builder to which to append `Attribute`s
    */
  def serverAddressAndPortForBuilder[F[_]](
      request: Request[F],
      forwarded: Option[Forwarded],
      scheme: OriginalScheme,
  )(b: Attributes.Builder): b.type = {
    // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#setting-serveraddress-and-serverport-attributes

    def serverPort(maybePort: Option[Int]): Option[Attribute[Long]] =
      maybePort
        .map(_.toLong)
        .orElse(portFromScheme(scheme.value))
        .map(ServerAttributes.ServerPort.apply)

    forwarded
      .flatMap(findFirstInForwarded(_, _.maybeHost))
      .map[b.type] { host =>
        b += ServerAttributes.ServerAddress(host.host.value)
        b ++= serverPort(host.port)
      }
      .orElse[b.type] {
        // parsing not currently supported, but if we know it exists then we
        // know not to keep checking other things
        request.headers
          .get[`X-Forwarded-Host`]
          .map { xfh =>
            b += ServerAttributes.ServerAddress(xfh.host)
            b ++= serverPort(xfh.port)
          }
      }
      .orElse[b.type] {
        request.httpVersion.major match {
          case 2 | 3 /* in case eventually supported by http4s */ =>
            // in HTTP/2 implementation, :authority pseudo-header is used to
            //   populate `Request#uri.authority` (at least by ember)
            request.uri.authority.map { authority =>
              b += ServerAttributes.ServerAddress(authority.host.value)
              b ++= serverPort(authority.port)
            }
          case _ => None
        }
      }
      .orElse[b.type] {
        request.headers
          .get[Host]
          .map { host =>
            b += ServerAttributes.ServerAddress(host.host)
            b ++= serverPort(host.port)
          }
      }
      .getOrElse(b)
  }

  /** @param request the client's request
    * @param forwarded the `Forwarded` header, if present in the request.
    *                  Because it is used in the creation of several
    *                  `Attribute`s, it is parsed and provided separately.
    *                  If not provided in this parameter, it will not be read
    *                  from `request`.
    * @param scheme the original scheme of the request
    * @return the `server.address` and `server.port` `Attributes`
    */
  def serverAddressAndPort[F[_]](
      request: Request[F],
      forwarded: Option[Forwarded],
      scheme: OriginalScheme,
  ): Attributes =
    serverAddressAndPortForBuilder(request, forwarded, scheme)(Attributes.newBuilder).result()

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

  /** @return the `url.scheme` `Attribute` */
  def urlScheme(scheme: OriginalScheme): Option[Attribute[String]] =
    scheme.value.map(s => UrlAttributes.UrlScheme(s.value))
}
