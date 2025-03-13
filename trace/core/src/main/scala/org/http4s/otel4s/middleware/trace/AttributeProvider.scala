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

import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

import scala.collection.immutable
import scala.collection.immutable.ArraySeq

/** Provides attributes for spans using requests and responses.
  *
  * It is RECOMMENDED that callers pass `Request`s and `Response`s that have
  * had their bodies replaced with empty `Stream`s (by calling
  * `.withBodyStream(Stream.empty)`).
  *
  * @note Implementations MUST NOT access request or response bodies.
  */
trait AttributeProvider {

  /** Provides attributes for a span using the given request.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def requestAttributes[F[_]](request: Request[F]): Attributes

  /** Provides attributes for a span using the given response.
    *
    * It is RECOMMENDED that callers pass `Response`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access response body.
    */
  def responseAttributes[F[_]](response: Response[F]): Attributes

  /** Provides attributes for a span based on a given exception. */
  def exceptionAttributes(cause: Throwable): Attributes

  /** @return an `AttributeProvider` that provides the attributes from this and
    *         another `AttributeProvider`
    */
  def and(that: AttributeProvider): AttributeProvider = that match {
    case AttributeProvider.Empty => this
    case AttributeProvider.Multi(providers) =>
      AttributeProvider.Multi(this +: providers)
    case _ => AttributeProvider.Multi(ArraySeq(this, that))
  }
}

object AttributeProvider {

  private[trace] object Empty extends AttributeProvider {
    def requestAttributes[F[_]](request: Request[F]): Attributes = Attributes.empty
    def responseAttributes[F[_]](response: Response[F]): Attributes = Attributes.empty
    def exceptionAttributes(cause: Throwable): Attributes = Attributes.empty

    override def and(that: AttributeProvider): AttributeProvider = that
  }

  /** Base trait for a wrapper around multiple [[`AttributeProvider`]]s. */
  private[trace] trait Multi extends AttributeProvider {
    protected def providers: Seq[AttributeProvider]
  }

  private[trace] object Multi {
    private[this] final class Impl(protected val providers: Seq[AttributeProvider]) extends Multi {
      def requestAttributes[F[_]](request: Request[F]): Attributes =
        providers.foldLeft(Attributes.empty)(_ ++ _.requestAttributes(request))
      def responseAttributes[F[_]](response: Response[F]): Attributes =
        providers.foldLeft(Attributes.empty)(_ ++ _.responseAttributes(response))
      def exceptionAttributes(cause: Throwable): Attributes =
        providers.foldLeft(Attributes.empty)(_ ++ _.exceptionAttributes(cause))

      override def and(that: AttributeProvider): AttributeProvider = that match {
        case Empty => this
        case Multi(ps) => new Impl(providers ++ ps)
        case _ => new Impl(providers :+ that)
      }
    }

    def apply(providers: Seq[AttributeProvider]): Multi = new Impl(providers)

    /** Extracts the [[`AttributeProvider`]]s from a `Multi` instance. */
    def unapply(multi: Multi): Some[Seq[AttributeProvider]] = Some(multi.providers)
  }

  private[this] final class Const(private val attributes: Attributes) extends AttributeProvider {
    def requestAttributes[F[_]](request: Request[F]): Attributes =
      attributes
    def responseAttributes[F[_]](response: Response[F]): Attributes =
      Attributes.empty
    def exceptionAttributes(cause: Throwable): Attributes =
      Attributes.empty

    override def and(that: AttributeProvider): AttributeProvider = that match {
      case const: Const => new Const(attributes ++ const.attributes)
      case other => super.and(other)
    }
  }

  /** @return an `AttributeProvider` that provides the given `Attribute`s */
  def const(attributes: immutable.Iterable[Attribute[_]]): AttributeProvider =
    if (attributes.isEmpty) Empty
    else new Const(attributes.to(Attributes))

  /** @return an `AttributeProvider` that provides the given `Attribute`s */
  def const(attributes: Attribute[_]*): AttributeProvider =
    const(attributes)

  /** Provides an `Attribute` containing this middleware's version. */
  val middlewareVersion: AttributeProvider =
    const(TypedAttributes.middlewareVersion)
}
