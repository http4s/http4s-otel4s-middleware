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
import munit.Location
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

class SpanDataProviderTest extends FunSuite {
  import SpanDataProviderTest._

  test("{Attribute,SpanData}Provider#and") {
    def check(
        provider: AttributeProvider,
        keys: Seq[String],
        last: String,
    )(implicit loc: Location): Unit = {
      val expected =
        keys.map(k => Attribute(s"test.key.$k", 1L)).to(Attributes) +
          Attribute("test.last", last)

      assertEquals(provider.requestAttributes(null), expected, "requestAttributes")
      assertEquals(provider.responseAttributes(null), expected, "responseAttributes")
      assertEquals(provider.exceptionAttributes(null), expected, "exceptionAttributes")
    }

    val a = new SimpleAttributeProvider("a")
    val b = new SimpleAttributeProvider("b")
    val c = new SimpleAttributeProvider("c")
    val d = new SimpleAttributeProvider("d")

    check(a.and(b).and(c).and(d), Seq("a", "b", "c", "d"), "d")
    check(a.and(b).and(c.and(d)), Seq("a", "b", "c", "d"), "d")
    check(a.and(b.and(c).and(d)), Seq("a", "b", "c", "d"), "d")
    check(a.and(b.and(c.and(d))), Seq("a", "b", "c", "d"), "d")

    def checkSpan(
        provider: SpanDataProvider,
        name: String,
        keys: Seq[String],
        last: String,
    )(implicit loc: Location): Unit = {
      assertEquals(provider.spanName(null, null.asInstanceOf[provider.Shared]), name, "spanName")
      check(provider, keys, last)
    }

    val e = new SimpleSpanDataProvider("e")
    val f = new SimpleSpanDataProvider("f")
    val g = new SimpleSpanDataProvider("g")
    val h = new SimpleSpanDataProvider("h")

    checkSpan(e.and(f).and(g).and(h), "e", Seq("e", "f", "g", "h"), "h")
    checkSpan(e.and(f).and(g.and(h)), "e", Seq("e", "f", "g", "h"), "h")
    checkSpan(e.and(f.and(g).and(h)), "e", Seq("e", "f", "g", "h"), "h")
    checkSpan(e.and(f.and(g.and(h))), "e", Seq("e", "f", "g", "h"), "h")

    checkSpan(e.and(a).and(f).and(b), "e", Seq("a", "b", "e", "f"), "b")
    checkSpan(e.and(a).and(f.and(b)), "e", Seq("a", "b", "e", "f"), "b")
    checkSpan(e.and(a.and(f).and(b)), "e", Seq("a", "b", "e", "f"), "b")
    checkSpan(e.and(a.and(f.and(b))), "e", Seq("a", "b", "e", "f"), "b")
  }
}

object SpanDataProviderTest {
  private[this] def attr(name: String): Attributes =
    Attributes(Attribute(s"test.key.$name", 1L), Attribute("test.last", name))

  private final class SimpleAttributeProvider(name: String) extends AttributeProvider {
    def requestAttributes[F[_]](request: Request[F]): Attributes = attr(name)
    def responseAttributes[F[_]](response: Response[F]): Attributes = attr(name)
    def exceptionAttributes(cause: Throwable): Attributes = attr(name)
  }

  private final class SimpleSpanDataProvider(name: String) extends SpanDataProvider {
    type Shared = Null
    def processSharedData[F[_]](request: Request[F]): Null = null
    def spanName[F[_]](request: Request[F], sharedProcessedData: Null): String = name
    def requestAttributes[F[_]](request: Request[F], sharedProcessedData: Null): Attributes =
      attr(name)
    def responseAttributes[F[_]](response: Response[F]): Attributes = attr(name)
    def exceptionAttributes(cause: Throwable): Attributes = attr(name)
  }
}
