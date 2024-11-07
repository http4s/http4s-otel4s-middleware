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

class PerRequestTracingFilterTest extends FunSuite {
  import PerRequestTracingFilterTest._

  test("and") {
    assertEquals(run(PRTF.alwaysTrace.and(PRTF.alwaysTrace)), true)
    assertEquals(run(PRTF.alwaysTrace.and(PRTF.neverTrace)), false)
    assertEquals(run(PRTF.neverTrace.and(PRTF.alwaysTrace)), false)
    assertEquals(run(PRTF.neverTrace.and(PRTF.neverTrace)), false)
  }

  test("or") {
    assertEquals(run(PRTF.alwaysTrace.or(PRTF.alwaysTrace)), true)
    assertEquals(run(PRTF.alwaysTrace.or(PRTF.neverTrace)), true)
    assertEquals(run(PRTF.neverTrace.or(PRTF.alwaysTrace)), true)
    assertEquals(run(PRTF.neverTrace.or(PRTF.neverTrace)), false)
  }

  test("default filter always traces") {
    assert(PRTF.default eq PRTF.alwaysTrace)
  }
}

object PerRequestTracingFilterTest {
  private val PRTF: PerRequestTracingFilter.type = PerRequestTracingFilter
  private val defaultPrelude = Request().requestPrelude

  private def run(prtf: PerRequestTracingFilter): Boolean =
    prtf(defaultPrelude).isEnabled
}
