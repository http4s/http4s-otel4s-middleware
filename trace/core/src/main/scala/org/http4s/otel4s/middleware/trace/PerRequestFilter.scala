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

import scala.annotation.unchecked.uncheckedStable

/** Determines whether a feature is enabled for a given request and its
  * response.
  */
trait PerRequestFilter {

  /** @return whether the feature is enabled for the given request and its
    *         response
    */
  def apply(request: RequestPrelude): PerRequestConfig

  /** @return a `PerRequestFilter` that returns `Enabled` when both `this` and
    *         `that` return `Enabled`
    */
  def and(that: PerRequestFilter): PerRequestFilter =
    req => apply(req).and(that(req))

  /** @return a `PerRequestFilter` that returns `Enabled` when either `this` or
    *         `that` returns `Enabled`
    */
  def or(that: PerRequestFilter): PerRequestFilter =
    req => apply(req).or(that(req))
}

object PerRequestFilter {

  /** A filter that always enables a given feature. */
  val alwaysEnabled: PerRequestFilter = _ => PerRequestConfig.Enabled

  /** A filter that always disables a given feature. */
  val neverEnabled: PerRequestFilter = _ => PerRequestConfig.Disabled

  @uncheckedStable
  @deprecated("use `alwaysEnabled` instead", since = "http4s-otel4s-middleware 0.14.0")
  def alwaysTrace: PerRequestFilter = alwaysEnabled

  @uncheckedStable
  @deprecated("use `neverEnabled` instead", since = "http4s-otel4s-middleware 0.14.0")
  def neverTrace: PerRequestFilter = neverEnabled

  @deprecated(
    "there is no reasonable default for arbitrary features",
    since = "http4s-otel4s-middleware 0.14.0",
  )
  def default: PerRequestFilter = alwaysEnabled
}
