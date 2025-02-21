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

import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor

/** Redacts URIs to remove sensitive information. */
trait UriRedactor extends PathRedactor with QueryRedactor {

  /** Redacts the username and password from a URI's authority. */
  protected final def redactUserInfo(authority: Uri.Authority): Uri.Authority =
    authority.userInfo.fold(authority) { info =>
      authority.copy(userInfo =
        Some(
          Uri.UserInfo(
            username = redact.REDACTED,
            password = info.password.map(_ => redact.REDACTED),
          )
        )
      )
    }

  /** @return a redacted authority, or `None` if the entire authority is
    *         sensitive
    */
  def redactAuthority(authority: Uri.Authority): Option[Uri.Authority] =
    Some(redactUserInfo(authority))

  /** @return a redacted fragment, or `None` if the entire fragment is
    *         sensitive
    */
  def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment]

  /** @return a URI redacted by individually redacting it authority, path,
    *         query and fragment
    */
  final def redactFull(uri: Uri): Uri =
    uri.copy(
      authority = uri.authority.flatMap(redactAuthority),
      path = redactPath(uri.path),
      query = redactQuery(uri.query),
      fragment = uri.fragment.flatMap(redactFragment),
    )
}

object UriRedactor {

  /** A `UriRedactor` that only redacts the username and password from a URI's
    * authority.
    */
  trait OnlyRedactUserInfo
      extends UriRedactor
      with PathRedactor.NeverRedact
      with QueryRedactor.NeverRedact {
    def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment] =
      Some(fragment)
  }
}
