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
package otel4s.middleware.client

import com.comcast.ip4s._
import munit.FunSuite
import munit.Location
import org.http4s.Request
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute

class TypedClientAttributesTest extends FunSuite {
  private[this] def checkOpt[A](
      attr: TypedClientAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedClientAttributes), expected)

  test("serverAddress") {
    def check(host: Option[Uri.Host], expected: Option[String])(implicit loc: Location): Unit =
      checkOpt(_.serverAddress(host), expected.map(Attribute("server.address", _)))

    check(None, None)
    check(Some(Uri.Host.unsafeFromString("localhost")), Some("localhost"))
    check(Some(Uri.Host.unsafeFromString("example.com")), Some("example.com"))
  }

  test("serverPort") {
    def check(port: Option[Port], url: Uri, expected: Option[Long])(implicit loc: Location): Unit =
      checkOpt(_.serverPort(port, url), expected.map(Attribute("server.port", _)))

    check(Port.fromInt(4140), uri"http://localhost:8080", Some(8080L))
    check(Port.fromInt(9443), uri"https://example.com:8443", Some(8443L))
    check(Port.fromInt(4140), uri"http://localhost", Some(80L))
    check(Port.fromInt(9443), uri"https://example.com", Some(443L))
    check(Port.fromInt(9443), uri"https://[2001:db8::1]:8443", Some(8443L))
    check(Port.fromInt(9443), uri"custom://example.com:9000", Some(9000L))
    check(Port.fromInt(9443), uri"custom://example.com", Some(9443L))
    check(Port.fromInt(4140), uri"//localhost", Some(4140L))
    check(Port.fromInt(4140), uri"/items", Some(4140L))
    check(None, uri"http://localhost:8080", Some(8080L))
    check(None, uri"https://example.com", Some(443L))
    check(None, uri"//localhost", None)
    check(None, uri"custom://example.com", None)
  }

  test("urlScheme") {
    def check(
        scheme: Option[Uri.Scheme],
        expected: Option[String],
    )(implicit loc: Location): Unit =
      checkOpt(_.urlScheme(scheme), expected.map(Attribute("url.scheme", _)))

    check(None, None)
    check(Some(Uri.Scheme.http), Some("http"))
    check(Some(Uri.Scheme.https), Some("https"))
  }

  List(
    (uri"/items", "example.test:8443", Some("example.test"), Some(8443L)),
    (uri"/items", "example.test", Some("example.test"), None),
    (uri"/items", "192.0.2.1:8080", Some("192.0.2.1"), Some(8080L)),
    (uri"/items", "[2001:db8::1]:8443", Some("2001:db8::1"), Some(8443L)),
    (uri"https:/items", "example.test:8443", Some("example.test"), Some(8443L)),
    (uri"https:/items", "example.test", Some("example.test"), Some(443L)),
    (uri"https://origin.test/items", "other.test:9000", Some("origin.test"), Some(443L)),
    (uri"//origin.test/items", "other.test:9000", Some("origin.test"), None),
    (uri"/items", "bad host:invalid", None, None),
  ).foreach { case (uri, host, address, port) =>
    test(s"request destination falls back to Host only without URI authority: $uri / $host") {
      val request = Request[Option](uri = uri).putHeaders("Host" -> host)
      assertEquals(TypedClientAttributes.serverAddress(request).map(_.value), address)
      assertEquals(TypedClientAttributes.serverPort(request).map(_.value), port)

      val connected = request.withAttribute(
        Request.Keys.ConnectionInfo,
        Request.Connection(
          SocketAddress(ip"127.0.0.1", port"12345"),
          SocketAddress(ip"192.0.2.1", port"4140"),
          secure = false,
        ),
      )
      assertEquals(
        TypedClientAttributes.serverPort(connected).map(_.value),
        port.orElse(Some(4140L)),
      )
    }
  }

  test("request destination stays unknown without URI authority or Host") {
    val request = Request[Option](uri = uri"/items")
    assertEquals(TypedClientAttributes.serverAddress(request), None)
    assertEquals(TypedClientAttributes.serverPort(request), None)
  }
}
