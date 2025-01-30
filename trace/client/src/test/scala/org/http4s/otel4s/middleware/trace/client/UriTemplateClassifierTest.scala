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
package otel4s.middleware.trace.client

import munit.FunSuite
import org.http4s.syntax.literals._

class UriTemplateClassifierTest extends FunSuite {
  test("orElse") {
    val a: UriTemplateClassifier =
      uri => Option.when(uri.path == Uri.Path.Root / "a")("/a")
    val b: UriTemplateClassifier =
      uri => Option.when(uri.path == Uri.Path.Root / "b")("/b")
    val classifier = a.orElse(b)
    def check(uri: Uri, expected: Option[String]): Unit =
      assertEquals(classifier.classify(uri), expected)

    check(uri"https://example.com/a", Some("/a"))
    check(uri"https://example.com/b", Some("/b"))
    check(uri"https://example.com/c", None)
  }
}
