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
package otel4s.middleware.trace
package server

import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Ipv4Address
import com.comcast.ip4s.Ipv6Address
import com.comcast.ip4s.Port
import com.comcast.ip4s.SocketAddress
import munit.FunSuite
import munit.Location
import org.http4s.headers.Forwarded
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class TypedServerTraceAttributesTest extends FunSuite {
  private[this] def checkOpt[A](
      attr: TypedServerTraceAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedServerTraceAttributes), expected)

  private[this] def checkAttr(
      attr: TypedServerTraceAttributes.type => Attributes,
      expected: Attributes,
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedServerTraceAttributes), expected)

  test("clientAddress") {
    def check(
        headers: Headers,
        remote: Option[SocketAddress[IpAddress]],
        expected: Option[String],
    )(implicit loc: Location): Unit = {
      val conn = remote.map { addr =>
        Request.Connection(
          local = SocketAddress(Ipv4Address.fromBytes(127, 0, 0, 1), Port.fromInt(4321).get),
          remote = addr,
          secure = true, // ignored
        )
      }
      val reqNoAttr = Request(headers = headers)
      val req = conn
        .fold(reqNoAttr)(reqNoAttr.withAttribute(Request.Keys.ConnectionInfo, _))
      checkOpt(
        _.clientAddress(req, headers.get[Forwarded]),
        expected.map(Attribute("client.address", _)),
      )
    }

    val f1 = Header.Raw(ci"Forwarded", "for=10.0.0.5")
    val f2 = Header.Raw(ci"Forwarded", "for=\"10.0.0.5:8080\"")
    val f3 = Header.Raw(ci"Forwarded", "for=\"[2001:db8:cafe::17]\"")
    val f4 = Header.Raw(ci"Forwarded", "for=\"[2001:db8:cafe::17]:4711\"")
    val f5 = Header.Raw(ci"Forwarded", "for=10.0.0.5, for=\"[2001:db8:cafe::17]\"")
    val f6 = Header.Raw(ci"Forwarded", "for=\"10.0.0.5:8080\", for=\"[2001:db8:cafe::17]\"")
    val f7 = Header.Raw(ci"Forwarded", "for=10.0.0.5, for=\"[2001:db8:cafe::17]:4711\"")
    val f8 = Header.Raw(ci"Forwarded", "for=\"_example\", for=10.0.0.5")
    val f9 = Header.Raw(ci"Forwarded", "for=\"_example\", for=\"10.0.0.5:8080\"")
    val f10 = Header.Raw(ci"Forwarded", "by=\"10.0.0.5:8080\"")
    val f11 = Header.Raw(ci"Forwarded", "by=192.168.3.3;for=10.0.0.5")
    val f12 = Header.Raw(ci"Forwarded", "by=\"192.168.3.3:42\";for=\"10.0.0.5:8080\"")
    val xff1 = Header.Raw(ci"X-Forwarded-For", "10.0.0.5")
    val xff2 = Header.Raw(ci"X-Forwarded-For", "2001:db8:cafe::17")
    val xff3 = Header.Raw(ci"X-Forwarded-For", "10.0.0.5, 2001:db8:cafe::17")
    val xff4 = Header.Raw(ci"X-Forwarded-For", "unknown, 10.0.0.5")
    val a1 = SocketAddress(Ipv4Address.fromBytes(192, 168, 1, 1), Port.fromInt(1234).get)
    val a2 = SocketAddress(Ipv6Address.fromString("bad::cafe").get, Port.fromInt(5678).get)

    check(Headers(f1), None, Some("10.0.0.5"))
    check(Headers(f2), None, Some("10.0.0.5"))
    check(Headers(f3), None, Some("2001:db8:cafe::17"))
    check(Headers(f4), None, Some("2001:db8:cafe::17"))
    check(Headers(f5), None, Some("10.0.0.5"))
    check(Headers(f6), None, Some("10.0.0.5"))
    check(Headers(f7), None, Some("10.0.0.5"))
    check(Headers(f8), None, None)
    check(Headers(f8), Some(a1), None)
    check(Headers(f9), None, None)
    check(Headers(f9), Some(a2), None)
    check(Headers(f10), None, None)
    check(Headers(f10), Some(a1), Some("192.168.1.1"))
    check(Headers(f11), None, Some("10.0.0.5"))
    check(Headers(f12), None, Some("10.0.0.5"))
    check(Headers(xff1), None, Some("10.0.0.5"))
    check(Headers(xff2), None, Some("2001:db8:cafe::17"))
    check(Headers(xff3), None, Some("10.0.0.5"))
    check(Headers(xff4), None, None)
    check(Headers(xff4), Some(a1), None)
    check(Headers.empty, Some(a1), Some("192.168.1.1"))
    check(Headers.empty, Some(a2), Some("bad::cafe"))

    // combinations
    check(Headers.empty, None, None)
    check(Headers(f1, xff2), Some(a1), Some("10.0.0.5"))
    check(Headers(f3, xff1), Some(a2), Some("2001:db8:cafe::17"))
    check(Headers(xff1), Some(a1), Some("10.0.0.5"))
  }

  test("clientAddressAndPort") {
    def check(
        headers: Headers,
        remote: Option[SocketAddress[IpAddress]],
        expected: Attributes,
    )(implicit loc: Location): Unit = {
      val conn = remote.map { addr =>
        Request.Connection(
          local = SocketAddress(Ipv4Address.fromBytes(127, 0, 0, 1), Port.fromInt(4321).get),
          remote = addr,
          secure = true, // ignored
        )
      }
      val reqNoAttr = Request(headers = headers)
      val req = conn
        .fold(reqNoAttr)(reqNoAttr.withAttribute(Request.Keys.ConnectionInfo, _))
      checkAttr(_.clientAddressAndPort(req, headers.get[Forwarded]), expected)
    }

    val f1 = Header.Raw(ci"Forwarded", "for=10.0.0.5")
    val f2 = Header.Raw(ci"Forwarded", "for=\"10.0.0.5:8080\"")
    val f3 = Header.Raw(ci"Forwarded", "for=\"[2001:db8:cafe::17]\"")
    val f4 = Header.Raw(ci"Forwarded", "for=\"[2001:db8:cafe::17]:4711\"")
    val f5 = Header.Raw(ci"Forwarded", "for=10.0.0.5, for=\"[2001:db8:cafe::17]\"")
    val f6 = Header.Raw(ci"Forwarded", "for=\"10.0.0.5:8080\", for=\"[2001:db8:cafe::17]\"")
    val f7 = Header.Raw(ci"Forwarded", "for=10.0.0.5, for=\"[2001:db8:cafe::17]:4711\"")
    val f8 = Header.Raw(ci"Forwarded", "for=\"_example\", for=10.0.0.5")
    val f9 = Header.Raw(ci"Forwarded", "for=\"_example\", for=\"10.0.0.5:8080\"")
    val f10 = Header.Raw(ci"Forwarded", "by=\"10.0.0.5:8080\"")
    val f11 = Header.Raw(ci"Forwarded", "by=192.168.3.3;for=10.0.0.5")
    val f12 = Header.Raw(ci"Forwarded", "by=\"192.168.3.3:42\";for=\"10.0.0.5:8080\"")
    val xff1 = Header.Raw(ci"X-Forwarded-For", "10.0.0.5")
    val xff2 = Header.Raw(ci"X-Forwarded-For", "2001:db8:cafe::17")
    val xff3 = Header.Raw(ci"X-Forwarded-For", "10.0.0.5, 2001:db8:cafe::17")
    val xff4 = Header.Raw(ci"X-Forwarded-For", "unknown, 10.0.0.5")
    val a1 = SocketAddress(Ipv4Address.fromBytes(192, 168, 1, 1), Port.fromInt(1234).get)
    val a2 = SocketAddress(Ipv6Address.fromString("bad::cafe").get, Port.fromInt(5678).get)

    check(Headers(f1), None, Attributes(Attribute("client.address", "10.0.0.5")))
    check(
      Headers(f2),
      None,
      Attributes(Attribute("client.address", "10.0.0.5"), Attribute("client.port", 8080L)),
    )
    check(Headers(f3), None, Attributes(Attribute("client.address", "2001:db8:cafe::17")))
    check(
      Headers(f4),
      None,
      Attributes(Attribute("client.address", "2001:db8:cafe::17"), Attribute("client.port", 4711L)),
    )
    check(Headers(f5), None, Attributes(Attribute("client.address", "10.0.0.5")))
    check(
      Headers(f6),
      None,
      Attributes(Attribute("client.address", "10.0.0.5"), Attribute("client.port", 8080L)),
    )
    check(Headers(f7), None, Attributes(Attribute("client.address", "10.0.0.5")))
    check(Headers(f8), None, Attributes.empty)
    check(Headers(f8), Some(a1), Attributes.empty)
    check(Headers(f9), None, Attributes.empty)
    check(Headers(f9), Some(a2), Attributes.empty)
    check(Headers(f10), None, Attributes.empty)
    check(
      Headers(f10),
      Some(a1),
      Attributes(Attribute("client.address", "192.168.1.1"), Attribute("client.port", 1234L)),
    )
    check(Headers(f11), None, Attributes(Attribute("client.address", "10.0.0.5")))
    check(
      Headers(f12),
      None,
      Attributes(Attribute("client.address", "10.0.0.5"), Attribute("client.port", 8080L)),
    )
    check(Headers(xff1), None, Attributes(Attribute("client.address", "10.0.0.5")))
    check(Headers(xff2), None, Attributes(Attribute("client.address", "2001:db8:cafe::17")))
    check(Headers(xff3), None, Attributes(Attribute("client.address", "10.0.0.5")))
    check(Headers(xff4), None, Attributes.empty)
    check(Headers(xff4), Some(a1), Attributes.empty)
    check(
      Headers.empty,
      Some(a1),
      Attributes(Attribute("client.address", "192.168.1.1"), Attribute("client.port", 1234L)),
    )
    check(
      Headers.empty,
      Some(a2),
      Attributes(Attribute("client.address", "bad::cafe"), Attribute("client.port", 5678L)),
    )

    // combinations
    check(Headers.empty, None, Attributes.empty)
    check(Headers(f1, xff2), Some(a1), Attributes(Attribute("client.address", "10.0.0.5")))
    check(Headers(f3, xff1), Some(a2), Attributes(Attribute("client.address", "2001:db8:cafe::17")))
    check(Headers(xff1), Some(a1), Attributes(Attribute("client.address", "10.0.0.5")))
  }

  test("urlPath") {
    val redactor = new redact.PathRedactor {
      private[this] val allowedSegments = Set("users", "profile")
      private[this] val redactedSegment = Uri.Path.Segment(redact.REDACTED)
      def redactPath(path: Uri.Path): Uri.Path =
        Uri.Path(
          segments = path.segments.map { s =>
            if (allowedSegments.contains(s.encoded)) s else redactedSegment
          },
          absolute = path.absolute,
          endsWithSlash = path.endsWithSlash,
        )
    }
    def check(path: Uri.Path, expected: Option[String]): Unit =
      checkOpt(_.urlPath(path, redactor), expected.map(Attribute("url.path", _)))

    check(Uri.Path.empty, None)
    check(Uri.Path.Root, Some("/"))
    check(Uri.Path.Root / "users", Some("/users"))
    check(Uri.Path.Root / "users" / "295472d0-ef9e-48a7-84bd-100a4672ff87", Some("/users/REDACTED"))
    check(
      Uri.Path.Root / "users" / "295472d0-ef9e-48a7-84bd-100a4672ff87" / "profile",
      Some("/users/REDACTED/profile"),
    )
  }

  test("urlQuery") {
    val redactor = new redact.QueryRedactor {
      private[this] val forbiddenParams = Set("token")
      private[this] val someRedacted = Some(redact.REDACTED)
      def redactQuery(query: Query): Query =
        Query.fromVector {
          query.pairs.map {
            case (key, _: Some[_]) if forbiddenParams.contains(key) =>
              key -> someRedacted
            case p => p
          }
        }
    }
    def check(query: Query, expected: Option[String]): Unit =
      checkOpt(_.urlQuery(query, redactor), expected.map(Attribute("url.query", _)))

    check(Query.empty, None)
    check(Query.blank, Some(""))
    check(
      Query.fromPairs("userId" -> "295472d0-ef9e-48a7-84bd-100a4672ff87"),
      Some("userId=295472d0-ef9e-48a7-84bd-100a4672ff87"),
    )
    check(Query("token" -> None), Some("token"))
    check(
      Query("token" -> Some("46b76633577e52635123bd66b5341476cefeb046487ea85d6a7c2a38d3febfc0")),
      Some("token=REDACTED"),
    )
    check(
      Query.fromPairs(
        "userId" -> "295472d0-ef9e-48a7-84bd-100a4672ff87",
        "token" -> "46b76633577e52635123bd66b5341476cefeb046487ea85d6a7c2a38d3febfc0",
      ),
      Some("userId=295472d0-ef9e-48a7-84bd-100a4672ff87&token=REDACTED"),
    )
  }
}
