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
package otel4s.middleware.client

import cats.effect.IO
import munit.FunSuite
import org.http4s.dsl.Http4sDsl
import org.http4s.syntax.literals._

class UriTemplateClassifierTest extends FunSuite {
  test("forPathAndQuery") {
    val classifier = locally {
      val http4sDsl = Http4sDsl[IO]
      import http4sDsl._
      object Id extends QueryParamDecoderMatcher[String]("id")
      UriTemplateClassifier.matchingPathAndQuery {
        case (Root / "users", Id(_)) => "/users?id={userId}"
        case (Root / "users" / UUIDVar(_) / "profile", _) =>
          "/users/{userId}/profile"
        case (Root / "users" / UUIDVar(_), _) =>
          "/users/{userId}"
      }
    }
    def check(uri: Uri, expected: Option[String]): Unit =
      assertEquals(classifier.classify(uri), expected)

    check(uri"/users", None)
    check(uri"/users?id=295472d0-ef9e-48a7-84bd-100a4672ff87", Some("/users?id={userId}"))
    check(uri"/users?id=not-a-uuid", Some("/users?id={userId}"))
    check(uri"/users?id=1234&foo=bar", Some("/users?id={userId}"))
    check(uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87", Some("/users/{userId}"))
    check(uri"/users/not-a-uuid", None)
    check(
      uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87/profile?foo=bar",
      Some("/users/{userId}/profile"),
    )
    check(uri"/users/not-a-uuid/profile", None)
  }

  test("orElse") {
    val a = locally {
      val http4sDsl = Http4sDsl[IO]
      import http4sDsl._
      UriTemplateClassifier.matchingPathAndQuery { case (Root / "a", _) => "/a" }
    }
    val b = locally {
      val http4sDsl = Http4sDsl[IO]
      import http4sDsl._
      UriTemplateClassifier.matchingPathAndQuery { case (Root / "b", _) => "/b" }
    }
    val classifier = a.orElse(b)
    def check(uri: Uri, expected: Option[String]): Unit =
      assertEquals(classifier.classify(uri), expected)

    check(uri"https://example.com/a", Some("/a"))
    check(uri"https://example.com/b", Some("/b"))
    check(uri"https://example.com/c", None)
  }
}
