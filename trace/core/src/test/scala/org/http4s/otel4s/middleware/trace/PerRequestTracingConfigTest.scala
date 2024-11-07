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

class PerRequestTracingConfigTest extends FunSuite {
  import PerRequestTracingConfigTest._

  test("and") {
    assertEquals(PRTC.Enabled.and(PRTC.Enabled), PRTC.Enabled)
    assertEquals(PRTC.Enabled.and(PRTC.Disabled), PRTC.Disabled)
    assertEquals(PRTC.Disabled.and(PRTC.Enabled), PRTC.Disabled)
    assertEquals(PRTC.Disabled.and(PRTC.Disabled), PRTC.Disabled)
  }

  test("or") {
    assertEquals(PRTC.Enabled.or(PRTC.Enabled), PRTC.Enabled)
    assertEquals(PRTC.Enabled.or(PRTC.Disabled), PRTC.Enabled)
    assertEquals(PRTC.Disabled.or(PRTC.Enabled), PRTC.Enabled)
    assertEquals(PRTC.Disabled.or(PRTC.Disabled), PRTC.Disabled)
  }
}

object PerRequestTracingConfigTest {
  val PRTC: PerRequestTracingConfig.type = PerRequestTracingConfig
}
