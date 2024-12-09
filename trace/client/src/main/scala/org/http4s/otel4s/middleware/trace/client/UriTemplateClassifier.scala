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
package otel4s.middleware.trace.client

/** Determines the template of an
  * [[https://www.rfc-editor.org/rfc/rfc3986#section-4.2 absolute path reference]]
  * from a URI.
  *
  * @note This trait is used to provide the value for the experimental
  *       `url.template` `Attribute`, which is used in the span name even
  *       though the `url.template` `Attribute` is not included in the span
  *       because it is experimental. See
  *       [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client-experimental-attributes]]
  *       for more information.
  */
trait UriTemplateClassifier {
  def classify(url: Uri): Option[String]
}

object UriTemplateClassifier {

  /** A classifier that does not classify any URI templates. */
  val indeterminate: UriTemplateClassifier = _ => None
}
