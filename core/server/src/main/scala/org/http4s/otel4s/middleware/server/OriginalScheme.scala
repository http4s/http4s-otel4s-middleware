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

import org.http4s.Headers
import org.http4s.Uri
import org.http4s.headers.Forwarded
import org.http4s.headers.`X-Forwarded-Proto`

/** The original scheme used by a client in a request. */
final class OriginalScheme private (val value: Option[Uri.Scheme]) extends AnyVal

object OriginalScheme {

  /** @param forwarded the `Forwarded` header, if present in the request.
    *                  Because it is used in the creation of several
    *                  `Attribute`s, it is parsed and provided separately.
    *                  If not provided in this parameter, it will not be read
    *                  from `headers`.
    * @param headers the request's headers
    * @param url the request's URL
    * @return the original scheme used by the client, possibly forwarded by
    *         a `Forwarded` or `X-Forwarded-Proto` header
    */
  def apply(
      forwarded: Option[Forwarded],
      headers: Headers,
      url: Uri,
  ): OriginalScheme = {
    val scheme = forwarded
      .flatMap(findFirstInForwarded(_, _.maybeProto))
      .orElse(headers.get[`X-Forwarded-Proto`].map(_.scheme))
      .orElse(url.scheme)
    new OriginalScheme(scheme)
  }
}
