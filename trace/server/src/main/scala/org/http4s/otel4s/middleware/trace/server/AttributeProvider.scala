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
package trace.server

import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attributes

/** Provides attributes for spans using requests and responses.
  *
  * It is RECOMMENDED that callers pass `Request`s and `Response`s that have
  * had their bodies replaced with empty `Stream`s (by calling
  * `.withBodyStream(Stream.empty)`).
  *
  * @note Implementations MUST NOT access request or response bodies.
  */
trait AttributeProvider { self =>

  /** Provides attributes for a span using the given request.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def requestAttributes[F[_]](
      request: Request[F],
      routeClassifier: RouteClassifier,
      redactor: PathAndQueryRedactor,
      headersAllowedAsAttributes: Set[CIString],
  ): Attributes

  /** Provides attributes for a span using the given response.
    *
    * It is RECOMMENDED that callers pass `Response`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access response body.
    */
  def responseAttributes[F[_]](
      response: Response[F],
      headersAllowedAsAttributes: Set[CIString],
  ): Attributes

  /** Provides attributes for a span based on a given exception. */
  def exceptionAttributes(cause: Throwable): Attributes

  /** @return an `AttributeProvider` that provides the attributes from this and
    *         another `AttributeProvider`
    */
  def and(that: AttributeProvider): AttributeProvider =
    new AttributeProvider {
      def requestAttributes[F[_]](
          request: Request[F],
          routeClassifier: RouteClassifier,
          redactor: PathAndQueryRedactor,
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes =
        self.requestAttributes(
          request,
          routeClassifier,
          redactor,
          headersAllowedAsAttributes,
        ) ++
          that.requestAttributes(
            request,
            routeClassifier,
            redactor,
            headersAllowedAsAttributes,
          )

      def responseAttributes[F[_]](
          response: Response[F],
          headersAllowedAsAttributes: Set[CIString],
      ): Attributes =
        self.responseAttributes(response, headersAllowedAsAttributes) ++
          that.responseAttributes(response, headersAllowedAsAttributes)

      def exceptionAttributes(cause: Throwable): Attributes =
        self.exceptionAttributes(cause) ++ that.exceptionAttributes(cause)
    }
}
