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
package client

import com.comcast.ip4s.Port
import org.http4s.headers.Host
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes

import scala.annotation.unchecked.uncheckedStable

private[middleware] trait TypedClientAttributes extends TypedAttributes {

  /** @return the `server.address` `Attribute` */
  final def serverAddress(host: Uri.Host): Attribute[String] =
    ServerAttributes.ServerAddress(host.value)

  /** This is a required `Attribute`, but `Uri` does not guarantee that it is
    * populated. So we just hope that it's always there in practice.
    *
    * @return the `server.address` `Attribute`
    */
  final def serverAddress(host: Option[Uri.Host]): Option[Attribute[String]] =
    host.map(serverAddress)

  /** Uses the URI host, falling back to `Host` for requests without a URI authority. */
  final def serverAddress[F[_]](request: Request[F]): Option[Attribute[String]] =
    serverAddress(request.uri.host)
      .orElse(request.headers.get[Host].map(host => ServerAttributes.ServerAddress(host.host)))

  /** This is a required `Attribute`, but none of `Request#remote`, `Uri#port`
    * and `Uri#scheme` are guaranteed to be populated. So we just hope that at
    * least one of them is always there in practice.
    *
    * @return the `server.port` `Attribute`
    */
  final def serverPort(
      remotePort: Option[Port],
      url: Uri,
  ): Option[Attribute[Long]] =
    serverPort(remotePort, url.port, url.scheme)

  /** Uses the destination port before the connection port, with a `Host` port fallback when the URI
    * has no authority. Without an explicit port, only a known URI scheme supplies a default.
    */
  final def serverPort[F[_]](request: Request[F]): Option[Attribute[Long]] = {
    val port = request.uri.authority match {
      case Some(authority) => authority.port
      case None => request.headers.get[Host].flatMap(_.port)
    }
    serverPort(request.remotePort, port, request.uri.scheme)
  }

  private def serverPort(
      remotePort: Option[Port],
      port: Option[Int],
      scheme: Option[Uri.Scheme],
  ): Option[Attribute[Long]] =
    port
      .map(_.toLong)
      .orElse(portFromScheme(scheme))
      .orElse(remotePort.map(_.value.toLong))
      .map(ServerAttributes.ServerPort(_))

  /** @return the `url.scheme` `Attribute` */
  final def urlScheme(scheme: Uri.Scheme): Attribute[String] =
    UrlAttributes.UrlScheme(scheme.value)

  /** @return the `url.scheme` `Attribute` */
  final def urlScheme(scheme: Option[Uri.Scheme]): Option[Attribute[String]] =
    scheme.map(urlScheme)

  /** Methods for creating appropriate experimental `Attribute`s from typed
    * HTTP objects within an HTTP client.
    */
  @uncheckedStable
  def Experimental: TypedClientAttributes.Experimental =
    TypedClientAttributes._Experimental
}

/** Methods for creating appropriate `Attribute`s from typed HTTP objects
  * within an HTTP client.
  */
object TypedClientAttributes extends TypedClientAttributes {

  private val UrlTemplate = AttributeKey[String]("url.template")

  /** Methods for creating appropriate experimental `Attribute`s from typed
    * HTTP objects within an HTTP client.
    */
  sealed trait Experimental {

    /** @return the `url.template` `Attribute` */
    final def urlTemplate(url: Uri, classifier: UriTemplateClassifier): Option[Attribute[String]] =
      UrlTemplate.maybe(classifier.classify(url))
  }

  private object _Experimental extends Experimental
}
