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
