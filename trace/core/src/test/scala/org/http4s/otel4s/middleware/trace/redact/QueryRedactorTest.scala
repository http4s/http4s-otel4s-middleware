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
import munit.Location

class QueryRedactorTest extends FunSuite {
  private val standardRedactor = new QueryRedactor.RedactSensitive {}

  List(
    "X-Amz-Signature",
    "X-Amz-Credential",
    "X-Amz-Security-Token",
    "sig",
    "X-Goog-Signature",
  ).foreach { key =>
    test(s"default redactor replaces sensitive values and preserves their key: $key") {
      val query = Query(key -> Some("secret"), "color" -> Some("blue"))
      assertEquals(
        standardRedactor.redactQuery(query),
        Query(key -> Some("REDACTED"), "color" -> Some("blue")),
      )
    }
  }

  test("matching is case-sensitive") {
    val query =
      Query("SIG" -> Some("keep"), "x-amz-signature" -> Some("keep"), "sig" -> Some("secret"))
    assertEquals(
      standardRedactor.redactQuery(query),
      Query("SIG" -> Some("keep"), "x-amz-signature" -> Some("keep"), "sig" -> Some("REDACTED")),
    )
  }

  test("duplicates, ordering, empty values, and valueless parameters are preserved") {
    val query = Query(
      "sig" -> Some("first"),
      "color" -> Some("blue"),
      "sig" -> None,
      "sig" -> Some(""),
      "empty" -> Some(""),
      "flag" -> None,
      "color" -> Some("red"),
      "sig" -> Some("second"),
    )
    val expected = Query(
      "sig" -> Some("REDACTED"),
      "color" -> Some("blue"),
      "sig" -> None,
      "sig" -> Some("REDACTED"),
      "empty" -> Some(""),
      "flag" -> None,
      "color" -> Some("red"),
      "sig" -> Some("REDACTED"),
    )
    assertEquals(standardRedactor.redactQuery(query), expected)
    assertEquals(standardRedactor.redactQuery(expected), expected)
  }

  test("encoded parameter names are matched after decoding") {
    val query = Query.unsafeFromString("s%69g=secret%26data&color=a%2Bb&sig=second")
    assertEquals(
      standardRedactor.redactQuery(query),
      Query("sig" -> Some("REDACTED"), "color" -> Some("a+b"), "sig" -> Some("REDACTED")),
    )
  }

  test("unaffected queries retain their representation") {
    List(Query.empty, Query.blank, Query.unsafeFromString("%63olor=blue&flag&empty=&color=red"))
      .foreach { query =>
        assertEquals(standardRedactor.redactQuery(query).renderString, query.renderString)
      }
  }

  test("RedactSensitive allows extending the standard keys without changing the defaults") {
    val redactor = new QueryRedactor.RedactSensitive {
      override val sensitiveQueryParameters: Set[String] =
        QueryRedactor.DefaultSensitiveQueryParameters + "token"
    }
    val query = Query("token" -> Some("secret"), "sig" -> Some("signature"))
    assertEquals(
      redactor.redactQuery(query),
      Query("token" -> Some("REDACTED"), "sig" -> Some("REDACTED")),
    )
    assertEquals(
      standardRedactor.redactQuery(query),
      Query("token" -> Some("secret"), "sig" -> Some("REDACTED")),
    )
  }

  test("RedactSensitive uses the standard keys by default") {
    val redactor = new QueryRedactor.RedactSensitive {}
    assertEquals(redactor.sensitiveQueryParameters, QueryRedactor.DefaultSensitiveQueryParameters)
    val query = Query("token" -> Some("secret"), "sig" -> Some("signature"))
    assertEquals(
      redactor.redactQuery(query),
      Query("token" -> Some("secret"), "sig" -> Some("REDACTED")),
    )
  }

  test("RedactSensitive allows replacing the standard keys") {
    val redactor = new QueryRedactor.RedactSensitive {
      override val sensitiveQueryParameters: Set[String] = Set("token")
    }
    val query = Query("token" -> Some("secret"), "sig" -> Some("signature"))
    assertEquals(
      redactor.redactQuery(query),
      Query("token" -> Some("REDACTED"), "sig" -> Some("signature")),
    )
  }

  test("RedactSensitive allows an empty key set") {
    val redactor = new QueryRedactor.RedactSensitive {
      override val sensitiveQueryParameters: Set[String] = Set.empty
    }
    val query = Query("sig" -> Some("signature"))
    assertEquals(redactor.redactQuery(query), query)
  }

  test("QueryRedactor.NeverRedact") {
    val redactor = new QueryRedactor.NeverRedact {}

    def check(query: Query)(implicit loc: Location): Unit =
      assertEquals(redactor.redactQuery(query), query)

    check(Query.empty)
    check(Query.blank)
    check(Query("foo" -> None, "bar" -> None))
    check(Query("empty" -> None, "foo" -> Some("bar"), "baz" -> Some("qux")))
    check(Query("foo" -> Some("1"), "foo" -> Some("2")))
  }
}
