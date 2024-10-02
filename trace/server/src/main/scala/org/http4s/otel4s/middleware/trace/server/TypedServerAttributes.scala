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
package otel4s.middleware.trace.server

import com.comcast.ip4s.Port
import org.http4s.headers.`X-Forwarded-For`
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.semconv.attributes.ClientAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes

/** Methods for creating appropriate `Attribute`s from typed HTTP objects
  * within an HTTP server.
  */
object TypedServerAttributes {

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
