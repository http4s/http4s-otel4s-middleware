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

import org.http4s.Query
import org.http4s.Uri

/** Something that redacts URIs to remove sensitive information.
  *
  * @note It is highly recommended to use [[UriRedactor.ByParts]] for
  *       implementations, as it allows easier redaction of individual
  *       parts of the URI.
  *
  * @see [[UriRedactor.ByParts]]
  */
trait UriRedactor {

  /** Redacts a URI.
    *
    * @note [[UriRedactor.ByParts]] comes with a default implementation of this
    *       method
    * @return a redacted URI, or `None` if the entire URI is sensitive
    */
  def redact(uri: Uri): Option[Uri]

  /** Redacts a URI using this redactor, and then redacts the result using
    * `that` redactor.
    */
  final def andThen(that: UriRedactor): UriRedactor =
    uri => this.redact(uri).flatMap(that.redact)

  /** Redacts a URI using `that` redactor, and then redacts the result using
    * this redactor.
    */
  final def compose(that: UriRedactor): UriRedactor =
    that.andThen(this)
}

object UriRedactor {

  /** The string `"REDACTED"`, for use in URI redactors. */
  final val Redacted = "REDACTED"

  /** A `UriRedactor` that only redacts the userinfo part of the URI's authority. */
  val OnlyRedactUserInfo: UriRedactor =
    new ByParts {
      protected def redactPath(path: Uri.Path): Uri.Path = path
      protected def redactQuery(query: Query): Query = query
      protected def redactFragment(
          fragment: Uri.Fragment
      ): Option[Uri.Fragment] = Some(fragment)
    }

  /** A `UriRedactor` that always redacts the entire URI. */
  val RedactEntirely: UriRedactor = _ => None

  /** A [[`UriRedactor`]] that redacts individual parts of the URI separately. */
  trait ByParts extends UriRedactor {

    /** Redacts the username and password from a URI's authority. */
    protected final def redactUserInfo(authority: Uri.Authority): Uri.Authority =
      authority.userInfo.fold(authority) { info =>
        authority.copy(userInfo =
          Some(
            Uri.UserInfo(
              username = UriRedactor.Redacted,
              password = info.password.map(_ => UriRedactor.Redacted),
            )
          )
        )
      }

    /** @return a redacted authority, or `None` if the entire authority is
      *         sensitive
      */
    protected def redactAuthority(authority: Uri.Authority): Option[Uri.Authority] =
      Some(redactUserInfo(authority))

    /** @return a redacted path, or `Uri.Path.empty` if the entire path is
      *         sensitive
      */
    protected def redactPath(path: Uri.Path): Uri.Path

    /** @return a redacted query, or `Query.empty` if the entire query is
      *         sensitive
      */
    protected def redactQuery(query: Query): Query

    /** @return a redacted fragment, or `None` if the entire fragment is
      *         sensitive
      */
    protected def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment]

    /** @return an entire URI redacted using [[`redactAuthority`]],
      *         [[`redactPath`]], [[`redactQuery`]] and [[`redactFragment`]]
      */
    protected final def redactParts(uri: Uri): Uri = {
      val path = {
        val p = uri.path
        if (p == Uri.Path.empty) p
        else redactPath(p)
      }
      val query = {
        val q = uri.query
        if (q.isEmpty) q
        else redactQuery(q)
      }

      uri.copy(
        authority = uri.authority.flatMap(redactAuthority),
        path = path,
        query = query,
        fragment = uri.fragment.flatMap(redactFragment),
      )
    }

    def redact(uri: Uri): Option[Uri] =
      Some(redactParts(uri))
  }
}
