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
package otel4s.middleware.client

import scala.collection.immutable.ArraySeq

/** Determines the template of an
  * [[https://www.rfc-editor.org/rfc/rfc3986#section-4.2 absolute path reference]]
  * from a URI.
  *
  * @note This trait is used to provide the value for the experimental
  *       `url.template` `Attribute`. See
  *       [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client]]
  *       for more information. The value of the `url.template` `Attribute` is
  *       also used as part of the span name.
  */
trait UriTemplateClassifier {

  /** Determines the template of an absolute path reference from a URI.
    * A value of `None` indicates that the template could not be determined.
    */
  def classify(url: Uri): Option[String]

  /** @return a classifier which attempts to determine the template of a URI
    *         first using `this`, and then using `that` if `this.classify`
    *         returns `None`
    */
  def orElse(that: UriTemplateClassifier): UriTemplateClassifier = that match {
    case UriTemplateClassifier.Multi(classifiers) =>
      UriTemplateClassifier.Multi(this +: classifiers)
    case _ => UriTemplateClassifier.Multi(ArraySeq(this, that))
  }
}

object UriTemplateClassifier {

  private final case class Multi(classifiers: Seq[UriTemplateClassifier])
      extends UriTemplateClassifier {
    def classify(url: Uri): Option[String] =
      classifiers.view
        .map(_.classify(url))
        .find(_.isDefined)
        .flatten

    override def orElse(that: UriTemplateClassifier): UriTemplateClassifier = that match {
      case Multi(cs) => Multi(classifiers ++ cs)
      case _ => Multi(classifiers :+ that)
    }
  }

  /** A classifier that does not classify any URI templates. */
  val indeterminate: UriTemplateClassifier = _ => None
}
