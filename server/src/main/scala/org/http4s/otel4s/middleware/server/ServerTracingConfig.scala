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

package org.http4s.otel4s.middleware.server

import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.trace.SpanDataProvider

/** Tracing settings for [[ServerInstrumentation]]. */
sealed trait ServerTracingConfig {
  def spanDataProvider: SpanDataProvider
  def errorClassifier: ErrorClassifier
  def requestFilter: PerRequestFilter
  def reversePropagationFilter: PerRequestFilter

  /** Sets the provider for span names, attributes, and redaction. */
  def withSpanDataProvider(provider: SpanDataProvider): ServerTracingConfig

  /** Classifies response status errors. App and body exceptions take precedence. */
  def withErrorClassifier(classifier: ErrorClassifier): ServerTracingConfig

  /** Selects requests to trace. Checked each time the app runs. */
  def withRequestFilter(filter: PerRequestFilter): ServerTracingConfig

  /** Selects traced requests to propagate context to in response headers. Other headers are preserved. */
  def withReversePropagationFilter(filter: PerRequestFilter): ServerTracingConfig
}

object ServerTracingConfig {

  /** Enables tracing for all requests and treats 5xx responses as errors.
    * Response context propagation is disabled. Uses the given provider for span names,
    * attributes, and redaction.
    */
  def apply(spanDataProvider: SpanDataProvider): ServerTracingConfig = Impl(spanDataProvider)

  private final case class Impl(
      spanDataProvider: SpanDataProvider,
      errorClassifier: ErrorClassifier = ErrorClassifier.serverError,
      requestFilter: PerRequestFilter = PerRequestFilter.alwaysEnabled,
      reversePropagationFilter: PerRequestFilter = PerRequestFilter.neverEnabled,
  ) extends ServerTracingConfig {
    def withSpanDataProvider(provider: SpanDataProvider): ServerTracingConfig =
      copy(spanDataProvider = provider)
    def withErrorClassifier(classifier: ErrorClassifier): ServerTracingConfig =
      copy(errorClassifier = classifier)
    def withRequestFilter(filter: PerRequestFilter): ServerTracingConfig =
      copy(requestFilter = filter)
    def withReversePropagationFilter(filter: PerRequestFilter): ServerTracingConfig =
      copy(reversePropagationFilter = filter)
  }
}
