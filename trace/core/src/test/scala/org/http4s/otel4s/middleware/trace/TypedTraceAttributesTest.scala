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

package org.http4s.otel4s.middleware.trace

import com.comcast.ip4s.Port
import munit.FunSuite
import munit.Location
import org.http4s.Header
import org.http4s.Headers
import org.http4s.Method
import org.http4s.headers.`User-Agent`
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class TypedTraceAttributesTest extends FunSuite {
  private[this] def checkAttr[A](
      attr: TypedTraceAttributes.type => Attribute[A],
      expected: Attribute[A],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedTraceAttributes), expected)

  private[this] def checkOpt[A](
      attr: TypedTraceAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedTraceAttributes), expected)

  private[this] def check(
      attr: TypedTraceAttributes.type => Attributes,
      expected: Attributes,
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedTraceAttributes), expected)

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

  test("networkPeerPort") {
    checkAttr(_.networkPeerPort(Port.fromInt(8080).get), Attribute("network.peer.port", 8080L))
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
      _.httpRequestHeaders(
        Headers("foo" -> "1", "bar" -> "2"),
        HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.Elide),
      ),
      Attributes(Attribute("http.request.header.foo", Seq("1"))),
    )
    check(
      _.httpRequestHeaders(
        Headers("foo" -> "1", "bar" -> "2"),
        HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.DefaultReplacement),
      ),
      Attributes(
        Attribute("http.request.header.foo", Seq("1")),
        Attribute("http.request.header.bar", Seq("<REDACTED>")),
      ),
    )
    check(
      _.httpRequestHeaders(
        Headers("Cookie" -> "abc123"),
        HeaderRedactor(Set(ci"Cookie"), HeaderRedactor.Behavior.Elide),
      ),
      Attributes(Attribute("http.request.header.cookie", Seq("<REDACTED>"))),
    )
  }

  test("httpResponseHeaders") {
    check(
      _.httpResponseHeaders(
        Headers("foo" -> "1", "bar" -> "2"),
        HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.Elide),
      ),
      Attributes(Attribute("http.response.header.foo", Seq("1"))),
    )
    check(
      _.httpResponseHeaders(
        Headers("foo" -> "1", "bar" -> "2"),
        HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.DefaultReplacement),
      ),
      Attributes(
        Attribute("http.response.header.foo", Seq("1")),
        Attribute("http.response.header.bar", Seq("<REDACTED>")),
      ),
    )
    check(
      _.httpResponseHeaders(
        Headers("Set-Cookie" -> "abc123"),
        HeaderRedactor(Set(ci"Set-Cookie"), HeaderRedactor.Behavior.Elide),
      ),
      Attributes(Attribute("http.response.header.set-cookie", Seq("<REDACTED>"))),
    )
  }
}
