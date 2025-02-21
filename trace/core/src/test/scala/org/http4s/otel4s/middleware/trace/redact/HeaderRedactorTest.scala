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
package otel4s.middleware.trace.redact

import munit.FunSuite
import org.typelevel.ci.CIStringSyntax

class HeaderRedactorTest extends FunSuite {
  test("#alsoAllowingUnredacted") {
    val r = HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.Elide)
      .alsoAllowingUnredacted(Set(ci"bar"))
    assertEquals(r.headersAllowedUnredacted, Set(ci"foo", ci"bar"))
  }

  test("Behavior.Elide") {
    val r = HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.Elide)
    assertEquals(r.redactHeaderValues(ci"foo", Seq("1", "2")), Some(Seq("1", "2")))
    assertEquals(r.redactHeaderValues(ci"bar", Seq("1", "2")), None)
  }

  test("Behavior.DefaultReplacement") {
    val r = HeaderRedactor(Set(ci"bar"), HeaderRedactor.Behavior.DefaultReplacement)
    assertEquals(r.redactHeaderValues(ci"foo", Seq("1", "2")), Some(Seq("<REDACTED>")))
    assertEquals(r.redactHeaderValues(ci"bar", Seq("1", "2")), Some(Seq("1", "2")))
  }

  test("""Behavior.ReplaceWith("anything")""") {
    val r = HeaderRedactor(Set(ci"foo"), HeaderRedactor.Behavior.ReplaceWith("anything"))
    assertEquals(r.redactHeaderValues(ci"foo", Seq("1", "2")), Some(Seq("1", "2")))
    assertEquals(r.redactHeaderValues(ci"bar", Seq("1", "2")), Some(Seq("anything")))
  }

  test(".default") {
    val r = HeaderRedactor.default
    assertEquals(
      r.redactHeaderValues(ci"X-Forwarded-For", Seq("192.168.1.1", "aaaaaa")),
      Some(Seq("192.168.1.1", "aaaaaa")),
    )
    assertEquals(r.redactHeaderValues(ci"foo", Seq("1", "2")), None)
  }

  test(".replaceAll") {
    val r = HeaderRedactor.replaceAll
    assertEquals(
      r.redactHeaderValues(ci"X-Forwarded-For", Seq("192.168.1.1", "aaaaaa")),
      Some(Seq("<REDACTED>")),
    )
    assertEquals(r.redactHeaderValues(ci"foo", Seq("1", "2")), Some(Seq("<REDACTED>")))
  }
}
