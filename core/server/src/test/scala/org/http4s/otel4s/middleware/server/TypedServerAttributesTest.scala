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
package otel4s.middleware.server

import munit.FunSuite
import munit.Location
import org.http4s.headers.Forwarded
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class TypedServerAttributesTest extends FunSuite {
  private[this] def checkOpt[A](
      attr: TypedServerAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedServerAttributes), expected)

  private[this] def checkAttr(
      attr: TypedServerAttributes.type => Attributes,
      expected: Attributes,
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedServerAttributes), expected)

  test("httpRoute") {
    // this is a terrible classifier, but it's just for testing that it gets used
    val classifier: RouteClassifier =
      req => Option.when(req.method == Method.GET)(req.uri.path.renderString)
    def check(method: Method, uri: Uri, expected: Option[String]): Unit =
      checkOpt(
        _.httpRoute(Request(method, uri), classifier),
        expected.map(Attribute("http.route", _)),
      )

    check(Method.GET, uri"/test", Some("/test"))
    check(Method.POST, uri"/test", None)
  }

  test("serverAddressAndPort") {
    def check(
        headers: Headers,
        uri: Uri,
        httpVersion: HttpVersion,
        scheme: OriginalScheme,
        expected: Attributes,
    )(implicit loc: Location): Unit = {
      val req = Request(headers = headers, uri = uri, httpVersion = httpVersion)
      checkAttr(_.serverAddressAndPort(req, headers.get[Forwarded], scheme), expected)
    }

    val f1 = Header.Raw(ci"Forwarded", "host=example.com")
    val f2 = Header.Raw(ci"Forwarded", "host=\"example.com:8080\"")
    val f3 = Header.Raw(ci"Forwarded", "by=\"_example\";host=example.com")
    val f4 = Header.Raw(ci"Forwarded", "by=\"_example\";host=\"example.com:8080\"")
    val f5 = Header.Raw(ci"Forwarded", "by=\"_example\"")
    val xfh1 = Header.Raw(ci"X-Forwarded-Host", "typelevel.org")
    val xfh2 = Header.Raw(ci"X-Forwarded-Host", "typelevel.org:4140")
    val h1 = Header.Raw(ci"Host", "http4s.org")
    val h2 = Header.Raw(ci"Host", "http4s.org:25565")
    val u1 = uri"http://opentelemetry.io"
    val u2 = uri"https://opentelemetry.io"
    val u3 = uri"https://opentelemetry.io:1080"
    val os1 = OriginalScheme(None, Headers.empty, Uri())
    val os2 = OriginalScheme(None, Headers.empty, uri"http:")
    val os3 = OriginalScheme(None, Headers.empty, uri"https:")

    check(
      Headers(f1),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "example.com")),
    )
    check(
      Headers(f2),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "example.com"), Attribute("server.port", 8080L)),
    )
    check(
      Headers(f3),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "example.com")),
    )
    check(
      Headers(f4),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "example.com"), Attribute("server.port", 8080L)),
    )
    check(Headers(f5), Uri(), HttpVersion.`HTTP/1.1`, os1, Attributes.empty)
    check(
      Headers(xfh1),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "typelevel.org")),
    )
    check(
      Headers(xfh2),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "typelevel.org"), Attribute("server.port", 4140L)),
    )
    check(
      Headers(h1),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "http4s.org")),
    )
    check(
      Headers(h2),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os1,
      Attributes(Attribute("server.address", "http4s.org"), Attribute("server.port", 25565L)),
    )
    check(
      Headers.empty,
      u1,
      HttpVersion.`HTTP/2`,
      os2, // can't ever get `os1` in actuality, so not testing it
      Attributes(Attribute("server.address", "opentelemetry.io"), Attribute("server.port", 80L)),
    )
    check(
      Headers.empty,
      u2,
      HttpVersion.`HTTP/2`,
      os3, // can't ever get `os1` in actuality, so not testing it
      Attributes(Attribute("server.address", "opentelemetry.io"), Attribute("server.port", 443L)),
    )
    check(
      Headers.empty,
      u3,
      HttpVersion.`HTTP/2`,
      os1,
      Attributes(Attribute("server.address", "opentelemetry.io"), Attribute("server.port", 1080L)),
    )

    // combinations
    check(
      Headers(f1, xfh1, h1),
      Uri(),
      HttpVersion.`HTTP/1.1`,
      os2,
      Attributes(Attribute("server.address", "example.com"), Attribute("server.port", 80L)),
    )
    check(
      Headers(f3, h2),
      Uri(),
      HttpVersion.`HTTP/2`,
      os3,
      Attributes(Attribute("server.address", "example.com"), Attribute("server.port", 443L)),
    )
    check(
      Headers(f2, xfh2),
      u1,
      HttpVersion.`HTTP/2`,
      os1,
      Attributes(Attribute("server.address", "example.com"), Attribute("server.port", 8080L)),
    )
    check(
      Headers(xfh1, h2),
      u1,
      HttpVersion.`HTTP/1.1`,
      os2,
      Attributes(Attribute("server.address", "typelevel.org"), Attribute("server.port", 80L)),
    )
    check(
      Headers(h1),
      u3,
      HttpVersion.`HTTP/1.1`,
      os3,
      Attributes(Attribute("server.address", "http4s.org"), Attribute("server.port", 443L)),
    )
    check(
      Headers(h1),
      u3,
      HttpVersion.`HTTP/2`,
      os3,
      Attributes(Attribute("server.address", "opentelemetry.io"), Attribute("server.port", 1080L)),
    )
  }

  test("urlScheme") {
    def check(uri: Uri, expected: Option[String])(implicit loc: Location): Unit =
      checkOpt(
        _.urlScheme(OriginalScheme(None, Headers.empty, uri)),
        expected.map(Attribute("url.scheme", _)),
      )

    check(Uri(), None)
    check(uri"http:", Some("http"))
    check(uri"https:", Some("https"))
  }
}
