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

import munit.FunSuite
import org.typelevel.ci._

class HeadersAllowedAsAttributesTest extends FunSuite {
  test("`union` contains all elements of `this` and `that`") {
    val a = HeadersAllowedAsAttributes(Set(ci"foo", ci"bar"), Set(ci"baz", ci"qux"))
    val b = HeadersAllowedAsAttributes(Set(ci"bar", ci"baz"), Set(ci"foo", ci"baz"))
    val c = a | b
    assertEquals(c.request, Set(ci"foo", ci"bar", ci"baz"))
    assertEquals(c.response, Set(ci"foo", ci"baz", ci"qux"))
  }

  test("`union` returns `this` when `that` is empty") {
    val a = HeadersAllowedAsAttributes(Set(ci"foo", ci"bar"))
    assert((a | HeadersAllowedAsAttributes.none) eq a)
  }

  test("`union` deduplicates shared references") {
    val a = HeadersAllowedAsAttributes(Set(ci"foo")) |
      HeadersAllowedAsAttributes(Set(ci"bar"))
    assert(a.request eq a.response)
  }
}
