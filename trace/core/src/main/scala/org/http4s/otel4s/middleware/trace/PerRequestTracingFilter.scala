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

/** Determines whether a given request and its response should be traced. */
trait PerRequestTracingFilter {

  /** @return whether the given request and its response should be traced. */
  def apply(request: RequestPrelude): PerRequestTracingConfig

  /** @return a `PerRequestTracingFilter` that returns `Enabled` when both
    *         `this` and `that` return `Enabled`
    */
  def and(that: PerRequestTracingFilter): PerRequestTracingFilter =
    req => apply(req).and(that(req))

  /** @return a `PerRequestTracingFilter` that returns `Enabled` when either
    *         `this` or `that` returns `Enabled`
    */
  def or(that: PerRequestTracingFilter): PerRequestTracingFilter =
    req => apply(req).or(that(req))
}

object PerRequestTracingFilter {

  /** A filter that always enables tracing. */
  val alwaysTrace: PerRequestTracingFilter = _ => PerRequestTracingConfig.Enabled

  /** A filter that always disables tracing. */
  val neverTrace: PerRequestTracingFilter = _ => PerRequestTracingConfig.Disabled

  /** The default filter, which always enables tracing. */
  def default: PerRequestTracingFilter = alwaysTrace
}
