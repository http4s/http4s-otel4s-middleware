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

package org.http4s.otel4s.middleware

import munit.FunSuite
import org.http4s.Query
import org.http4s.Uri
import org.http4s.syntax.literals._

class UriRedactorTest extends FunSuite {
  test("UriRedactor.OnlyRedactUserInfo redacts username and password") {
    val r1 = UriRedactor.OnlyRedactUserInfo
      .redact(uri"http://user:Password1@localhost/")
    assertEquals(r1, Some(uri"http://REDACTED:REDACTED@localhost/"))

    val r2 = UriRedactor.OnlyRedactUserInfo
      .redact(uri"http://user@localhost/")
    assertEquals(r2, Some(uri"http://REDACTED@localhost/"))
  }

  test("UriRedactor.RedactEntirely redacts everything") {
    assertEquals(UriRedactor.RedactEntirely.redact(uri"http://localhost/"), None)
  }

  test("UriRedactor.ByParts redacts by parts") {
    val redactor = new UriRedactor.ByParts {
      override protected def redactAuthority(authority: Uri.Authority): Option[Uri.Authority] =
        None
      protected def redactPath(path: Uri.Path): Uri.Path =
        path"/REDACTED"
      protected def redactQuery(query: Query): Query =
        Query.fromMap(Map("REDACTED" -> Seq("REDACTED")))
      protected def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment] =
        Some("REDACTED")
    }

    val r1 = redactor.redact(uri"http://localhost/foo/bar?baz=qux&baz2=qux2#stuff")
    assertEquals(r1, Some(uri"http:/REDACTED?REDACTED=REDACTED#REDACTED"))

    val r2 = redactor.redact(uri"https:")
    assertEquals(r2, Some(uri"https:"))
  }

  test("UriRedactor#andThen and UriRedactor#compose") {
    val excluded = "foo"
    abstract class QueryRedactor extends UriRedactor.ByParts {
      protected def redactPath(path: Uri.Path): Uri.Path = path
      protected def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment] = None
    }
    val redactor1 = new QueryRedactor {
      protected def redactQuery(query: Query): Query =
        query.filterNot(_._1 == excluded)
    }
    val redactor2 = new QueryRedactor {
      protected def redactQuery(query: Query): Query =
        if (query.containsQueryParam(excluded)) Query.empty
        else query
    }
    val r1andThen2 = redactor1.andThen(redactor2)
    val r2compose1 = redactor2.compose(redactor1)
    val r2andThen1 = redactor2.andThen(redactor1)
    val r1compose2 = redactor1.compose(redactor2)

    val uri1 = uri"https://example.com?foo=bar&baz=qux"
    val uri2 = uri"https://example.com?baz=qux"
    val uri3 = uri"https://example.com"

    assertEquals(r1andThen2.redact(uri1), Some(uri2))
    assertEquals(r2compose1.redact(uri1), Some(uri2))
    assertEquals(r2andThen1.redact(uri1), Some(uri3))
    assertEquals(r1compose2.redact(uri1), Some(uri3))

    assertEquals(r1andThen2.redact(uri2), Some(uri2))
    assertEquals(r2compose1.redact(uri2), Some(uri2))
    assertEquals(r2andThen1.redact(uri2), Some(uri2))
    assertEquals(r1compose2.redact(uri2), Some(uri2))
  }
}
