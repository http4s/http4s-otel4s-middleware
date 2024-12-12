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
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Ipv6Address
import com.comcast.ip4s.Port
import munit.FunSuite
import munit.Location
import org.http4s.headers.`User-Agent`
import org.typelevel.ci._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class TypedAttributesTest extends FunSuite {
  import TypedAttributesTest.CustomException

  private[this] def checkAttr[A](
      attr: TypedAttributes.type => Attribute[A],
      expected: Attribute[A],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedAttributes), expected)

  private[this] def checkOpt[A](
      attr: TypedAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedAttributes), expected)

  private[this] def check(
      attr: TypedAttributes.type => Attributes,
      expected: Attributes,
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedAttributes), expected)

  test("errorType(Throwable)") {
    def check(t: String => Throwable, expected: String)(implicit loc: Location): Unit =
      checkAttr(_.errorType(t("unused")), Attribute("error.type", expected))

    check(new IllegalArgumentException(_), "java.lang.IllegalArgumentException")
    check(new IllegalStateException(_), "java.lang.IllegalStateException")
    check(new Error(_), "java.lang.Error")
    check(
      new CustomException.Impl(_),
      "org.http4s.otel4s.middleware.TypedAttributesTest$CustomException$Impl",
    )
    check(new CustomException(_) {}, "org.http4s.otel4s.middleware.TypedAttributesTest$$anon$1")
  }

  test("errorType(Status)") {
    def check(status: Status, expected: String)(implicit loc: Location): Unit =
      checkAttr(_.errorType(status), Attribute("error.type", expected))

    check(Status.Gone, "410")
    check(Status.ImATeapot, "418")
    check(Status.BadGateway, "502")
    check(Status.ServiceUnavailable, "503")
  }

  test("httpRequestMethod") {
    def check(method: Method, expected: String)(implicit loc: Location): Unit =
      checkAttr(_.httpRequestMethod(method), Attribute("http.request.method", expected))

    def unsafeMethod(name: String): Method =
      Method.fromString(name).toTry.get

    check(Method.GET, "GET")
    check(unsafeMethod("GET"), "GET")
    check(unsafeMethod("GeT"), "_OTHER")
    check(unsafeMethod("NOT-A-METHOD"), "_OTHER")
  }

  test("httpRequestMethodOriginal") {
    def check(method: Method, expected: String)(implicit loc: Location): Unit =
      checkAttr(
        _.httpRequestMethodOriginal(method),
        Attribute("http.request.method_original", expected),
      )

    def unsafeMethod(name: String): Method =
      Method.fromString(name).toTry.get

    check(Method.GET, "GET")
    check(unsafeMethod("GET"), "GET")
    check(unsafeMethod("GeT"), "GeT")
    check(unsafeMethod("NOT-A-METHOD"), "NOT-A-METHOD")
  }

  test("httpResponseStatusCode") {
    def check(status: Status, expected: Long)(implicit loc: Location): Unit =
      checkAttr(_.httpResponseStatusCode(status), Attribute("http.response.status_code", expected))

    check(Status.Processing, 102L)
    check(Status.NoContent, 204L)
    check(Status.SeeOther, 303L)
    check(Status.MethodNotAllowed, 405L)
    check(Status.GatewayTimeout, 504L)
  }

  test("networkPeerAddress") {
    def check(ip: IpAddress, expected: String)(implicit loc: Location): Unit =
      checkAttr(_.networkPeerAddress(ip), Attribute("network.peer.address", expected))

    check(Ipv4Address.fromBytes(192, 168, 1, 1), "192.168.1.1")
    check(Ipv6Address.Loopback, "::1")
  }

  test("networkPeerPort") {
    checkAttr(_.networkPeerPort(Port.fromInt(8080).get), Attribute("network.peer.port", 8080L))
  }

  test("networkProtocolVersion") {
    def check(httpVersion: HttpVersion, expected: String)(implicit loc: Location): Unit =
      checkAttr(
        _.networkProtocolVersion(httpVersion),
        Attribute("network.protocol.version", expected),
      )

    check(HttpVersion.`HTTP/0.9`, "0.9")
    check(HttpVersion.`HTTP/1.0`, "1.0")
    check(HttpVersion.`HTTP/1.1`, "1.1")
    check(HttpVersion.`HTTP/2`, "2")
    check(HttpVersion.`HTTP/3`, "3")
  }

  test("userAgentOriginal") {
    val userAgent = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:132.0) Gecko/20100101 Firefox/132.0"
    val header = Header.Raw(`User-Agent`.name, userAgent)
    checkOpt(
      _.userAgentOriginal(Headers(header)),
      Some(Attribute("user_agent.original", userAgent)),
    )
  }

  test("httpRequestHeaders") {
    check(
      _.httpRequestHeaders(Headers("foo" -> "1", "bar" -> "2"), Set(ci"foo")),
      Attributes(Attribute("http.request.header.foo", Seq("1"))),
    )
    check(
      _.httpRequestHeaders(Headers("Cookie" -> "abc123"), Set(ci"Cookie")),
      Attributes(Attribute("http.request.header.cookie", Seq("<REDACTED>"))),
    )
  }

  test("httpResponseHeaders") {
    check(
      _.httpResponseHeaders(Headers("foo" -> "1", "bar" -> "2"), Set(ci"foo")),
      Attributes(Attribute("http.response.header.foo", Seq("1"))),
    )
    check(
      _.httpResponseHeaders(Headers("Set-Cookie" -> "abc123"), Set(ci"Set-Cookie")),
      Attributes(Attribute("http.response.header.set-cookie", Seq("<REDACTED>"))),
    )
  }
}

object TypedAttributesTest {
  abstract class CustomException(message: String) extends Exception(message)
  object CustomException {
    final class Impl(message: String) extends CustomException(message)
  }
}
