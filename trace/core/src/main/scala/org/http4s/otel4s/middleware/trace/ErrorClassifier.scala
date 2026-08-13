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

trait ErrorClassifier {
  def isError(status: Status): Boolean

  def and(that: ErrorClassifier): ErrorClassifier =
    status => isError(status) && that.isError(status)

  def or(that: ErrorClassifier): ErrorClassifier =
    status => isError(status) || that.isError(status)

  def excluding(statuses: Status*): ErrorClassifier = {
    val excluded = statuses.toSet
    status => !excluded.contains(status) && isError(status)
  }

  def included(statuses: Status*): ErrorClassifier = {
    val included = statuses.toSet
    status => included.contains(status) || isError(status)
  }
}

object ErrorClassifier {
  val default: ErrorClassifier = { status =>
    status.responseClass match {
      case Status.ClientError | Status.ServerError => true
      case _ => false
    }
  }

  val serverError: ErrorClassifier =
    _.responseClass == Status.ServerError

  val never: ErrorClassifier = _ => false
}
