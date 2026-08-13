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

class ErrorClassifierTest extends FunSuite {

  test("default classifies 4xx and 5xx as errors") {
    def check(status: Status, expected: Boolean): Unit =
      assertEquals(ErrorClassifier.default.isError(status), expected)

    check(Status.Ok, false)
    check(Status.Created, false)
    check(Status.NoContent, false)
    check(Status.MovedPermanently, false)
    check(Status.BadRequest, true)
    check(Status.Unauthorized, true)
    check(Status.Forbidden, true)
    check(Status.NotFound, true)
    check(Status.InternalServerError, true)
    check(Status.NotImplemented, true)
    check(Status.BadGateway, true)
    check(Status.ServiceUnavailable, true)
  }

  test("serverError classifies only 5xx as errors") {
    def check(status: Status, expected: Boolean): Unit =
      assertEquals(ErrorClassifier.serverError.isError(status), expected)

    check(Status.Ok, false)
    check(Status.BadRequest, false)
    check(Status.NotFound, false)
    check(Status.InternalServerError, true)
    check(Status.NotImplemented, true)
    check(Status.BadGateway, true)
    check(Status.ServiceUnavailable, true)
  }

  test("never classifies nothing as an error") {
    def check(status: Status): Unit =
      assertEquals(ErrorClassifier.never.isError(status), false)

    check(Status.Ok)
    check(Status.BadRequest)
    check(Status.InternalServerError)
  }

  test("and combines two classifiers with logical AND") {
    val classifier = ErrorClassifier.default.and(ErrorClassifier.serverError)

    // default = 4xx || 5xx, serverError = 5xx → AND = 5xx only
    assertEquals(classifier.isError(Status.Ok), false)
    assertEquals(classifier.isError(Status.BadRequest), false)
    assertEquals(classifier.isError(Status.NotFound), false)
    assertEquals(classifier.isError(Status.InternalServerError), true)
    assertEquals(classifier.isError(Status.BadGateway), true)
  }

  test("or combines two classifiers with logical OR") {
    val classifier = ErrorClassifier.serverError.or(ErrorClassifier.never)

    // serverError = 5xx, never = none → OR = 5xx only
    assertEquals(classifier.isError(Status.Ok), false)
    assertEquals(classifier.isError(Status.BadRequest), false)
    assertEquals(classifier.isError(Status.InternalServerError), true)
  }

  test("excluding removes specific statuses from the classifier") {
    val classifier = ErrorClassifier.default.excluding(Status.NotFound, Status.InternalServerError)

    assertEquals(classifier.isError(Status.Ok), false)
    assertEquals(classifier.isError(Status.BadRequest), true)
    assertEquals(classifier.isError(Status.NotFound), false)
    assertEquals(classifier.isError(Status.InternalServerError), false)
    assertEquals(classifier.isError(Status.BadGateway), true)
  }

  test("included adds specific statuses to the classifier") {
    val classifier = ErrorClassifier.serverError.included(Status.Ok, Status.BadRequest)

    assertEquals(classifier.isError(Status.Ok), true)
    assertEquals(classifier.isError(Status.BadRequest), true)
    assertEquals(classifier.isError(Status.NotFound), false)
    assertEquals(classifier.isError(Status.InternalServerError), true)
    assertEquals(classifier.isError(Status.BadGateway), true)
  }

  test("custom classifier created from a lambda") {
    val onlyNotFound: ErrorClassifier = _ == Status.NotFound

    assertEquals(onlyNotFound.isError(Status.Ok), false)
    assertEquals(onlyNotFound.isError(Status.BadRequest), false)
    assertEquals(onlyNotFound.isError(Status.NotFound), true)
    assertEquals(onlyNotFound.isError(Status.InternalServerError), false)
  }

  test("excluding on never classifier has no effect") {
    val classifier = ErrorClassifier.never.excluding(Status.InternalServerError)

    assertEquals(classifier.isError(Status.Ok), false)
    assertEquals(classifier.isError(Status.InternalServerError), false)
  }

  test("included on never classifier adds statuses") {
    val classifier = ErrorClassifier.never.included(Status.BadRequest)

    assertEquals(classifier.isError(Status.Ok), false)
    assertEquals(classifier.isError(Status.BadRequest), true)
    assertEquals(classifier.isError(Status.InternalServerError), false)
  }

  test("excluding and included can be chained") {
    val classifier =
      ErrorClassifier.default.excluding(Status.BadRequest).included(Status.Ok)

    assertEquals(classifier.isError(Status.Ok), true)
    assertEquals(classifier.isError(Status.BadRequest), false)
    assertEquals(classifier.isError(Status.NotFound), true)
    assertEquals(classifier.isError(Status.InternalServerError), true)
  }
}
