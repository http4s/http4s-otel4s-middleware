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
package otel4s.middleware.trace

import munit.FunSuite

class PerRequestFilterTest extends FunSuite {
  import PerRequestFilterTest._

  test("and") {
    assertEquals(run(PRF.alwaysEnabled.and(PRF.alwaysEnabled)), true)
    assertEquals(run(PRF.alwaysEnabled.and(PRF.neverEnabled)), false)
    assertEquals(run(PRF.neverEnabled.and(PRF.alwaysEnabled)), false)
    assertEquals(run(PRF.neverEnabled.and(PRF.neverEnabled)), false)
  }

  test("or") {
    assertEquals(run(PRF.alwaysEnabled.or(PRF.alwaysEnabled)), true)
    assertEquals(run(PRF.alwaysEnabled.or(PRF.neverEnabled)), true)
    assertEquals(run(PRF.neverEnabled.or(PRF.alwaysEnabled)), true)
    assertEquals(run(PRF.neverEnabled.or(PRF.neverEnabled)), false)
  }
}

object PerRequestFilterTest {
  private val PRF: PerRequestFilter.type = PerRequestFilter
  private val defaultPrelude = Request().requestPrelude

  private def run(prf: PerRequestFilter): Boolean =
    prf(defaultPrelude).isEnabled
}
