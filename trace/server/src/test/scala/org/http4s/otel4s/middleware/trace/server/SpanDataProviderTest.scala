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
package otel4s.middleware.trace.server

import munit.FunSuite
import munit.Location
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class SpanDataProviderTest extends FunSuite {
  import SpanDataProviderTest._

  private[this] def check(
      output: Attributes,
      keys: Seq[String],
      last: String,
  )(implicit loc: Location): Unit =
    assertEquals(
      output,
      keys.map(k => Attribute(s"test.key.$k", 1L)).to(Attributes) + Attribute("test.last", last),
    )

  test("and") {
    val a = new SimpleAttributeProvider("a")
    val b = new SimpleAttributeProvider("b")
    val c = new SimpleSpanDataProvider("c")
    val d = new SimpleSpanDataProvider("d")

    val cabd = c.and(a.and(b).and(d))
    assertEquals(cabd.spanName(null, null, null, null.asInstanceOf[cabd.Shared]), "c")
    check(cabd.requestAttributes(null, null, null, null), Seq("a", "b", "c", "d"), "d")
    check(cabd.responseAttributes(null, null), Seq("a", "b", "c", "d"), "d")
    check(cabd.exceptionAttributes(null), Seq("a", "b", "c", "d"), "d")

    val db = d.and(b)
    assertEquals(db.spanName(null, null, null, null.asInstanceOf[db.Shared]), "d")
    check(db.requestAttributes(null, null, null, null), Seq("b", "d"), "b")
    check(db.responseAttributes(null, null), Seq("b", "d"), "b")
    check(db.exceptionAttributes(null), Seq("b", "d"), "b")
  }
}

object SpanDataProviderTest {
  private[this] def attr(name: String): Attributes =
    Attributes(Attribute(s"test.key.$name", 1L), Attribute("test.last", name))

  private final class SimpleAttributeProvider(name: String) extends AttributeProvider {
    def requestAttributes[F[_]](
        request: Request[F],
        routeClassifier: RouteClassifier,
        redactor: PathAndQueryRedactor,
        headersAllowedAsAttributes: Set[CIString],
    ): Attributes = attr(name)
    def responseAttributes[F[_]](
        response: Response[F],
        headersAllowedAsAttributes: Set[CIString],
    ): Attributes = attr(name)
    def exceptionAttributes(cause: Throwable): Attributes =
      attr(name)
  }

  private final class SimpleSpanDataProvider(name: String) extends SpanDataProvider {
    type Shared = Null
    def processSharedData[F[_]](
        request: Request[F],
        routeClassifier: RouteClassifier,
        redactor: PathAndQueryRedactor,
    ): Null = null
    def spanName[F[_]](
        request: Request[F],
        routeClassifier: RouteClassifier,
        redactor: PathAndQueryRedactor,
        sharedProcessedData: Null,
    ): String = name
    def requestAttributes[F[_]](
        request: Request[F],
        routeClassifier: RouteClassifier,
        redactor: PathAndQueryRedactor,
        sharedProcessedData: Null,
        headersAllowedAsAttributes: Set[CIString],
    ): Attributes = attr(name)
    def responseAttributes[F[_]](
        response: Response[F],
        headersAllowedAsAttributes: Set[CIString],
    ): Attributes = attr(name)
    def exceptionAttributes(cause: Throwable): Attributes =
      attr(name)
  }
}
