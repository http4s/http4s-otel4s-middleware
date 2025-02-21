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

class OriginalSchemeTest extends FunSuite {
  test("OriginalScheme.apply") {
    def check(
        headers: Headers,
        url: Uri,
        expected: Option[Uri.Scheme],
    )(implicit loc: Location): Unit =
      assertEquals(OriginalScheme(headers.get[Forwarded], headers, url).value, expected)

    val f1 = Header.Raw(ci"Forwarded", "proto=http")
    val f2 = Header.Raw(ci"Forwarded", "proto=https")
    val f3 = Header.Raw(ci"Forwarded", "by=\"_example\"")
    val f4 = Header.Raw(ci"Forwarded", "by=\"_example\";proto=https")
    val xfp1 = Header.Raw(ci"X-Forwarded-Proto", "http")
    val xfp2 = Header.Raw(ci"X-Forwarded-Proto", "https")
    val u1 = uri"http:"
    val u2 = uri"https:"
    val u3 = Uri()

    check(Headers(f1), u3, Some(Uri.Scheme.http))
    check(Headers(f2), u3, Some(Uri.Scheme.https))
    check(Headers(f3), u3, None)
    check(Headers(f4), u3, Some(Uri.Scheme.https))
    check(Headers(xfp1), u3, Some(Uri.Scheme.http))
    check(Headers(xfp2), u3, Some(Uri.Scheme.https))
    check(Headers.empty, u1, Some(Uri.Scheme.http))
    check(Headers.empty, u2, Some(Uri.Scheme.https))
    check(Headers.empty, u3, None)

    // combinations
    check(Headers(f1, xfp2), u2, Some(Uri.Scheme.http))
    check(Headers(f3, xfp1), u2, Some(Uri.Scheme.http))
    check(Headers(xfp2), u1, Some(Uri.Scheme.https))
  }
}
