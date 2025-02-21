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
package otel4s.middleware.server

import cats.effect.IO
import munit.FunSuite
import org.http4s.dsl.Http4sDsl
import org.http4s.syntax.literals._

class RouteClassifierTest extends FunSuite {
  test("of") {
    val classifier = locally {
      val http4sDsl = Http4sDsl[IO]
      import http4sDsl._
      RouteClassifier.of[IO] {
        case POST -> Root / "users" => "/users"
        case (GET | PUT) -> Root / "users" / UUIDVar(_) / "profile" =>
          "/users/{userId}/profile"
        case (HEAD | DELETE) -> Root / "users" / UUIDVar(_) =>
          "/users/{userId}"
      }
    }
    def check(method: Method, uri: Uri, expected: Option[String]): Unit =
      assertEquals(classifier.classify(Request(method, uri).requestPrelude), expected)

    check(Method.POST, uri"/users", Some("/users"))
    check(Method.POST, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87", None)
    check(Method.POST, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87/profile", None)
    check(
      Method.GET,
      uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87/profile?foo=bar",
      Some("/users/{userId}/profile"),
    )
    check(Method.GET, uri"/users/not-a-uuid/profile", None)
    check(Method.GET, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87", None)
    check(
      Method.PUT,
      uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87/profile",
      Some("/users/{userId}/profile"),
    )
    check(Method.PUT, uri"/users/not-a-uuid/profile", None)
    check(Method.PUT, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87", None)
    check(Method.HEAD, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87", Some("/users/{userId}"))
    check(Method.HEAD, uri"/users/not-a-uuid", None)
    check(Method.HEAD, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87/profile", None)
    check(Method.HEAD, uri"/users", None)
    check(Method.DELETE, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87", Some("/users/{userId}"))
    check(Method.DELETE, uri"/users/not-a-uuid", None)
    check(Method.DELETE, uri"/users/295472d0-ef9e-48a7-84bd-100a4672ff87/profile", None)
    check(Method.DELETE, uri"/users", None)
  }

  test("orElse") {
    val a = locally {
      val http4sDsl = Http4sDsl[IO]
      import http4sDsl._
      RouteClassifier.of[IO] { case POST -> Root / "test" =>
        "/test"
      }
    }
    val b = locally {
      val http4sDsl = Http4sDsl[IO]
      import http4sDsl._
      RouteClassifier.of[IO] { case GET -> Root / "test" =>
        "/test"
      }
    }
    val classifier = a.orElse(b)
    def check(method: Method, uri: Uri, expected: Option[String]): Unit =
      assertEquals(classifier.classify(Request(method, uri).requestPrelude), expected)

    check(Method.POST, uri"/test", Some("/test"))
    check(Method.GET, uri"/test", Some("/test"))
    check(Method.DELETE, uri"/test", None)
    check(Method.POST, uri"/other", None)
    check(Method.GET, uri"/other", None)
  }
}
