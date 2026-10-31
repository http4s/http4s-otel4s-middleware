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
package otel4s.middleware
package trace

/** Compatibility policy for tracing. Prefer `org.http4s.otel4s.middleware.ErrorClassifier`. */
@deprecated(
  "Use `org.http4s.otel4s.middleware.ErrorClassifier` instead",
  since = "http4s-otel4s-middleware 0.20.0",
)
trait ErrorClassifier extends org.http4s.otel4s.middleware.ErrorClassifier {
  def isError(request: RequestPrelude, response: ResponsePrelude): Boolean

  def and(that: ErrorClassifier): ErrorClassifier =
    (request, response) => isError(request, response) && that.isError(request, response)

  def or(that: ErrorClassifier): ErrorClassifier =
    (request, response) => isError(request, response) || that.isError(request, response)

  override def excluding(statuses: Status*): ErrorClassifier = {
    val excluded = statuses.toSet
    (request, response) => !excluded.contains(response.status) && isError(request, response)
  }

  override def included(statuses: Status*): ErrorClassifier = {
    val included = statuses.toSet
    (request, response) => included.contains(response.status) || isError(request, response)
  }
}

@deprecated(
  "Use `org.http4s.otel4s.middleware.ErrorClassifier` instead",
  since = "http4s-otel4s-middleware 0.20.0",
)
object ErrorClassifier {
  private def fromCore(classifier: org.http4s.otel4s.middleware.ErrorClassifier): ErrorClassifier =
    (request, response) => classifier.isError(request, response)

  val default: ErrorClassifier =
    fromCore(org.http4s.otel4s.middleware.ErrorClassifier.default)

  val serverError: ErrorClassifier =
    fromCore(org.http4s.otel4s.middleware.ErrorClassifier.serverError)

  val never: ErrorClassifier =
    fromCore(org.http4s.otel4s.middleware.ErrorClassifier.never)
}
