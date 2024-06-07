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

package org.http4s.otel4s.middleware
package trace.internal

import org.http4s.RequestPrelude
import org.http4s.ResponsePrelude
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute

import scala.collection.immutable

/** Base trait for middleware builders, to deduplicate some methods (and
  * guarantee consistency between builders).
  */
private[trace] abstract class MiddlewareBuilder[MB[X[_]] <: MiddlewareBuilder[MB, X], F[_]] {
  protected val allowedRequestHeaders: Set[CIString]
  protected val allowedResponseHeaders: Set[CIString]
  protected val spanName: RequestPrelude => String
  protected val additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]]
  protected val additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]]
  protected val urlRedactor: UriRedactor

  /** @return a copy of this builder with one or more changed values */
  protected def copyShared(
      allowedRequestHeaders: Set[CIString] = this.allowedRequestHeaders,
      allowedResponseHeaders: Set[CIString] = this.allowedResponseHeaders,
      spanName: RequestPrelude => String = this.spanName,
      additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]] =
        this.additionalRequestAttributes,
      additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]] =
        this.additionalResponseAttributes,
      urlRedactor: UriRedactor = this.urlRedactor,
  ): MB[F]

  /** Sets which request headers are allowed to be made into `Attribute`s. */
  def withAllowedRequestHeaders(allowedHeaders: Set[CIString]): MB[F] =
    copyShared(allowedRequestHeaders = allowedHeaders)

  /** Sets which response headers are allowed to be made into `Attribute`s. */
  def withAllowedResponseHeaders(allowedHeaders: Set[CIString]): MB[F] =
    copyShared(allowedResponseHeaders = allowedHeaders)

  /** Sets how to derive the name of a span from a request. */
  def withServerSpanName(spanName: RequestPrelude => String): MB[F] =
    copyShared(spanName = spanName)

  /** Sets how to derive additional `Attribute`s to add to a span from a
    * request.
    */
  def withAdditionalRequestAttributes(
      additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]]
  ): MB[F] =
    copyShared(additionalRequestAttributes = additionalRequestAttributes)

  /** Sets how to derive additional `Attribute`s to add to a span from a
    * response.
    */
  def withAdditionalResponseAttributes(
      additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]]
  ): MB[F] =
    copyShared(additionalResponseAttributes = additionalResponseAttributes)

  /** Sets how to redact URLs before turning them into `Attribute`s. */
  def withUrlRedactor(urlRedactor: UriRedactor): MB[F] =
    copyShared(urlRedactor = urlRedactor)
}
