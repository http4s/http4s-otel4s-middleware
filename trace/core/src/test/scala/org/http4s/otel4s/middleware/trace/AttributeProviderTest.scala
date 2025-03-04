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
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class AttributeProviderTest extends FunSuite {
  test("const(Attribute[_])") {
    val p = AttributeProvider.const(Attribute("key", "value"))
    assertEquals(p.requestAttributes(null), Attributes(Attribute("key", "value")))
    assert(p.responseAttributes(null).isEmpty)
    assert(p.exceptionAttributes(null).isEmpty)
  }

  test("const(Attributes)") {
    val attributes = Attributes(Attribute("foo", "bar"))
    val p = AttributeProvider.const(attributes)
    assert(p.requestAttributes(null) eq attributes)
    assert(p.responseAttributes(null).isEmpty)
    assert(p.exceptionAttributes(null).isEmpty)
  }

  test("const(...).and(const(...))") {
    val p1 = AttributeProvider.const(Attribute("foo", "bar"))
    val p2 = AttributeProvider.const(Attribute("baz", "qux"))
    val p = p1.and(p2)
    assertEquals(
      p.requestAttributes(null),
      Attributes(Attribute("foo", "bar"), Attribute("baz", "qux")),
    )
    assert(p.responseAttributes(null).isEmpty)
    assert(p.exceptionAttributes(null).isEmpty)
    assert(p.requestAttributes(null) eq p.requestAttributes(null))
  }

  test("middlewareVersion") {
    val p = AttributeProvider.middlewareVersion
    assertEquals(
      p.requestAttributes(null),
      Attributes(
        Attribute(
          "org.http4s.otel4s.middleware.version",
          org.http4s.otel4s.middleware.BuildInfo.version,
        )
      ),
    )
    assert(p.responseAttributes(null).isEmpty)
    assert(p.exceptionAttributes(null).isEmpty)
  }
}
