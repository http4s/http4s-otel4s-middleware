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

package org.http4s.otel4s.middleware.client

import org.http4s.otel4s.middleware.ErrorClassifier
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.trace.SpanDataProvider

/** Tracing settings for [[ClientInstrumentation]]. */
sealed trait ClientTracingConfig {
  def spanDataProvider: SpanDataProvider
  def errorClassifier: ErrorClassifier
  def requestFilter: PerRequestFilter
  def propagationFilter: PerRequestFilter

  /** Sets the provider for span names, attributes, and redaction. */
  def withSpanDataProvider(provider: SpanDataProvider): ClientTracingConfig

  /** Classifies response status errors. Request and response body exceptions take precedence. */
  def withErrorClassifier(classifier: ErrorClassifier): ClientTracingConfig

  /** Selects requests to trace. Checked each time the response resource is acquired. */
  def withRequestFilter(filter: PerRequestFilter): ClientTracingConfig

  /** Selects traced requests to propagate context to. Other request headers are preserved. */
  def withPropagationFilter(filter: PerRequestFilter): ClientTracingConfig
}

object ClientTracingConfig {

  /** Enables tracing and context propagation for all requests. Treats 4xx and 5xx responses as errors.
    * Uses the given provider for span names, attributes, and redaction.
    */
  def apply(spanDataProvider: SpanDataProvider): ClientTracingConfig = Impl(spanDataProvider)

  private final case class Impl(
      spanDataProvider: SpanDataProvider,
      errorClassifier: ErrorClassifier = ErrorClassifier.default,
      requestFilter: PerRequestFilter = PerRequestFilter.alwaysEnabled,
      propagationFilter: PerRequestFilter = PerRequestFilter.alwaysEnabled,
  ) extends ClientTracingConfig {
    def withSpanDataProvider(provider: SpanDataProvider): ClientTracingConfig =
      copy(spanDataProvider = provider)
    def withErrorClassifier(classifier: ErrorClassifier): ClientTracingConfig =
      copy(errorClassifier = classifier)
    def withRequestFilter(filter: PerRequestFilter): ClientTracingConfig =
      copy(requestFilter = filter)
    def withPropagationFilter(filter: PerRequestFilter): ClientTracingConfig =
      copy(propagationFilter = filter)
  }
}
