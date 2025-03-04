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

import munit.FunSuite
import munit.Location
import org.http4s.client.middleware.Retry
import org.http4s.otel4s.middleware.client.UriTemplateClassifier
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute

class TypedClientTraceAttributesTest extends FunSuite {

  private[this] def checkAttr[A](
      attr: TypedClientTraceAttributes.type => Attribute[A],
      expected: Attribute[A],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedClientTraceAttributes), expected)

  private[this] def checkOpt[A](
      attr: TypedClientTraceAttributes.type => Option[Attribute[A]],
      expected: Option[Attribute[A]],
  )(implicit loc: Location): Unit =
    assertEquals(attr(TypedClientTraceAttributes), expected)

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

  test("Experimental.urlTemplate") {
    // this is a terrible classifier, but it's just for testing that it gets used
    val classifier: UriTemplateClassifier =
      url =>
        Option.when(url.path.segments.headOption.exists(_.encoded == "test")) {
          url.path.renderString
        }
    def check(uri: Uri, expected: Option[String]): Unit =
      checkOpt(
        _.Experimental.urlTemplate(uri, classifier),
        expected.map(Attribute("url.template", _)),
      )

    check(uri"/test", Some("/test"))
    check(uri"/foo", None)
  }
}
