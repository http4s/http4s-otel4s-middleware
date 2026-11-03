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
package otel4s.middleware.trace.redact

/** Redacts a URI or request query. */
trait QueryRedactor {

  /** @return a redacted URI or request query, or `Query.empty` if the entire
    *         query is sensitive
    */
  def redactQuery(query: Query): Query
}

object QueryRedactor {

  /** Sensitive query parameter keys from the OpenTelemetry HTTP semantic conventions.
    * Matching is case-sensitive. This list may evolve with the conventions.
    *
    * @see [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client-span]]
    */
  val DefaultSensitiveQueryParameters: Set[String] = Set(
    "X-Amz-Signature",
    "X-Amz-Credential",
    "X-Amz-Security-Token",
    "sig",
    "X-Goog-Signature",
  )

  /** Mixes sensitive-query redaction into a path or URI redactor.
    * Keys, duplicate parameters, ordering, and parameters without values are preserved.
    */
  trait RedactSensitive extends QueryRedactor {

    /** Case-sensitive query parameter keys whose values are redacted.
      * Override to replace the defaults; an empty set leaves queries unchanged.
      */
    def sensitiveQueryParameters: Set[String] = DefaultSensitiveQueryParameters

    override def redactQuery(query: Query): Query = {
      val sensitiveParameters = sensitiveQueryParameters
      val redacted = query.pairs.map {
        case (key, Some(_)) if sensitiveParameters.contains(key) => key -> Some(REDACTED)
        case pair => pair
      }
      if (redacted == query.pairs) query else Query.fromVector(redacted)
    }
  }

  /** A `QueryRedactor` that never redacts anything. */
  trait NeverRedact extends QueryRedactor {
    def redactQuery(query: Query): Query = query
  }
}
