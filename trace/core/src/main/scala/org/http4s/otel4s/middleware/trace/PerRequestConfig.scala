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

package org.http4s.otel4s.middleware.trace

/** Whether a feature is enabled for a given request and its response. */
sealed trait PerRequestConfig {

  /** Whether the feature is enabled for a given request and its response. */
  def isEnabled: Boolean

  /** `Enabled` if both `this` and `that` are `Enabled`. */
  def and(that: PerRequestConfig): PerRequestConfig

  /** `Enabled` if either `this` or `that` are `Enabled`. */
  def or(that: PerRequestConfig): PerRequestConfig
}

object PerRequestConfig {

  /** A feature is enabled for a given request and its response. */
  case object Enabled extends PerRequestConfig {
    def isEnabled: Boolean = true
    def and(that: PerRequestConfig): PerRequestConfig = that
    def or(that: PerRequestConfig): PerRequestConfig = this
  }

  /** A feature is disabled for a given request and its response. */
  case object Disabled extends PerRequestConfig {
    def isEnabled: Boolean = false
    def and(that: PerRequestConfig): PerRequestConfig = this
    def or(that: PerRequestConfig): PerRequestConfig = that
  }
}
