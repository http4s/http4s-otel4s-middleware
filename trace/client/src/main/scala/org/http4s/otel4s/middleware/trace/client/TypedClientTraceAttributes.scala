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
package trace
package client

import org.http4s.client.middleware.Retry
import org.http4s.otel4s.middleware.client.TypedClientAttributes
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.UrlAttributes

/** Methods for creating appropriate `Attribute`s for tracing from typed HTTP
  * objects within an HTTP client.
  */
object TypedClientTraceAttributes extends TypedClientAttributes with TypedTraceAttributes {

  /** @return the `http.request.resend_count` `Attribute` */
  def httpRequestResendCount[F[_]](request: Request[F]): Option[Attribute[Long]] =
    // `AttemptCountKey` is 1 for the initial request. Since we want to track
    //   resends, we subtract 1
    request.attributes
      .lookup(Retry.AttemptCountKey)
      .collect {
        case c if c > 1 => c - 1L
      }
      .map(HttpAttributes.HttpRequestResendCount(_))

  /** @return the `url.full` `Attribute` containing the URL after redacting it
    *         with the provided [[`UriRedactor`]].
    */
  def urlFull(unredacted: Uri, redactor: UriRedactor): Attribute[String] =
    UrlAttributes.UrlFull(redactor.redactFull(unredacted).renderString)
}
