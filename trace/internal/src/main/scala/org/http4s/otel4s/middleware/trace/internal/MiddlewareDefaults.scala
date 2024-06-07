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
package trace
package internal

import org.http4s.RequestPrelude
import org.http4s.ResponsePrelude
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute

import scala.collection.immutable

/** Default values for middlewares. */
private[trace] object MiddlewareDefaults {
  def allowedRequestHeaders: Set[CIString] =
    TypedAttributes.Headers.defaultAllowedHeaders
  def allowedResponseHeaders: Set[CIString] =
    TypedAttributes.Headers.defaultAllowedHeaders
  val additionalRequestAttributes: RequestPrelude => immutable.Iterable[Attribute[_]] =
    _ => Nil
  val additionalResponseAttributes: ResponsePrelude => immutable.Iterable[Attribute[_]] =
    _ => Nil
  def urlRedactor: UriRedactor = UriRedactor.OnlyRedactUserInfo
}
