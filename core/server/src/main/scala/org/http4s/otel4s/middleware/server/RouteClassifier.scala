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
package otel4s.middleware.server

import scala.collection.immutable.ArraySeq

/** Determines the route within the application from a request.
  *
  * @note This trait is used to provide the value for the `http.route`
  *       `Attribute`, which is the path template relative to the application
  *       root. If the application's routes are written as a `PartialFunction`,
  *       then the precise format of the path template is up to the implementer
  *       to decide. However, if a library with its own format is used to
  *       generate the `PartialFunction`, then that library's format should be
  *       used. The values returned by an implementation should have low
  *       cardinality. See
  *       [[https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server]]
  *       for more information. The value of the `http.route` `Attribute` is
  *       also used as part of the span name.
  */
trait RouteClassifier {

  /** Determines the route within the application from a request.
    * A value of `None` indicates that the route could not be determined.
    */
  def classify(request: RequestPrelude): Option[String]

  /** @return a classifier which attempts to determine the route of an
    *          application first using `this`, and then using `that` if
    *          `this.classify` returns `None`
    */
  def orElse(that: RouteClassifier): RouteClassifier = that match {
    case RouteClassifier.Multi(classifiers) =>
      RouteClassifier.Multi(this +: classifiers)
    case _ => RouteClassifier.Multi(ArraySeq(this, that))
  }
}

object RouteClassifier {

  private final case class Multi(classifiers: Seq[RouteClassifier]) extends RouteClassifier {
    def classify(request: RequestPrelude): Option[String] =
      classifiers.view
        .map(_.classify(request))
        .find(_.isDefined)
        .flatten

    override def orElse(that: RouteClassifier): RouteClassifier = that match {
      case Multi(cs) => Multi(classifiers ++ cs)
      case _ => Multi(classifiers :+ that)
    }
  }

  /** A classifier that is never able to classify any routes. */
  val indeterminate: RouteClassifier = _ => None

  /** Mirrors `HttpRoutes.of` for use with `Http4sDsl`. The `Request` passed to
    * the provided `PartialFunction` will only be populated by the values from
    * a `RequestPrelude`.
    */
  def of[F[_]](pf: PartialFunction[Request[F], String]): RouteClassifier = {
    val lifted = pf.lift
    (prelude: RequestPrelude) => {
      val req = Request[F](
        method = prelude.method,
        uri = prelude.uri,
        httpVersion = prelude.httpVersion,
        headers = prelude.headers,
      )
      lifted(req)
    }
  }
}
