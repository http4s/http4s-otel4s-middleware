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
package otel4s.middleware.trace.client

import com.comcast.ip4s.Port
import munit.FunSuite
import munit.Location
import org.http4s.client.middleware.Retry
import org.http4s.otel4s.middleware.redact.PathRedactor
import org.http4s.otel4s.middleware.redact.QueryRedactor
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute

class TypedClientAttributesTest extends FunSuite {

  private[this] def checkAttr[A](
      attr: TypedClientAttributes.type => Attribute[A],
      expected: Attribute[A],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedClientAttributes), expected)

  private[this] def checkOpt[A](
      attr: TypedClientAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedClientAttributes), expected)

  test("httpRequestResendCount") {
    def check(attemptCount: Int, expected: Option[Long])(implicit loc: Location): Unit = {
      val req = Request().withAttribute(Retry.AttemptCountKey, attemptCount)
      checkOpt(
        _.httpRequestResendCount(req),
        expected.map(Attribute("http.request.resend_count", _)),
      )
    }

    check(0, None)
    check(1, None)
    check(2, Some(1L))
    check(3, Some(2L))
  }

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

    check(Port.fromInt(4140), uri"http://localhost:8080", Some(4140L))
    check(Port.fromInt(4140), uri"//localhost", Some(4140L))
    check(None, uri"http://localhost:8080", Some(8080L))
    check(None, uri"https://example.com", Some(443L))
    check(None, uri"//localhost", None)
  }

  test("urlFull") {
    val redactor =
      new UriRedactor with PathRedactor.NeverRedact with QueryRedactor.NeverRedact {
        def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment] = Some(fragment)
      }
    def check(url: Uri, expected: String)(implicit loc: Location): Unit =
      checkAttr(_.urlFull(url, redactor), Attribute("url.full", expected))

    check(uri"http://localhost:8080", "http://localhost:8080")
    check(uri"https://example.com/", "https://example.com/")
    check(uri"https://example.com/foo?", "https://example.com/foo?")
    check(uri"http://unsafe.example.com?#", "http://unsafe.example.com?#")
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

}
