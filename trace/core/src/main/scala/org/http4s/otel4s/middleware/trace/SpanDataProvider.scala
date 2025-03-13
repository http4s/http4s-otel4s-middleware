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
package otel4s.middleware.trace

import org.typelevel.otel4s.Attributes

import scala.collection.immutable.ArraySeq

/** Provides a name and attributes for spans using requests and responses.
  *
  * It is RECOMMENDED that callers pass `Request`s and `Response`s that have
  * had their bodies replaced with empty `Stream`s (by calling
  * `.withBodyStream(Stream.empty)`).
  *
  * @note Implementations MUST NOT access request or response bodies.
  */
trait SpanDataProvider extends AttributeProvider {

  /** The type of shared processed data used to provide both the span name and
    * request `Attributes`.
    */
  type Shared

  /** Process data used to provide both the span name and request attributes.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def processSharedData[F[_]](request: Request[F]): Shared

  /** Provides the name for a span using the given request.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def spanName[F[_]](request: Request[F], sharedProcessedData: Shared): String

  /** Provides attributes for a span using the given request.
    *
    * It is RECOMMENDED that callers pass `Request`s that have had their bodies
    * replaced with empty `Stream`s (by calling `.withBodyStream(Stream.empty)`).
    *
    * @note Implementation MUST NOT access request body.
    */
  def requestAttributes[F[_]](request: Request[F], sharedProcessedData: Shared): Attributes

  final def requestAttributes[F[_]](request: Request[F]): Attributes =
    requestAttributes(request, processSharedData(request))

  /** Returns a `SpanDataProvider` that provides the attributes from this and
    * another [[`AttributeProvider`]].
    *
    * If `that` is a `SpanDataProvider`, it will not be used to provide
    * span names.
    */
  override def and(that: AttributeProvider): SpanDataProvider = that match {
    case AttributeProvider.Empty => this
    case AttributeProvider.Multi(providers) =>
      SpanDataProvider.Multi(this, providers)
    case _ => SpanDataProvider.Multi(this, ArraySeq(that))
  }
}

object SpanDataProvider {

  // it is crucial that `primary` is never an instance of `Multi`
  private final case class Multi(primary: SpanDataProvider, others: Seq[AttributeProvider])
      extends SpanDataProvider
      with AttributeProvider.Multi { self =>
    type Shared = primary.Shared
    protected def providers: Seq[AttributeProvider] = primary +: others
    def processSharedData[F[_]](request: Request[F]): Shared =
      primary.processSharedData(request)
    def spanName[F[_]](request: Request[F], sharedProcessedData: Shared): String =
      primary.spanName(request, sharedProcessedData)
    def requestAttributes[F[_]](request: Request[F], sharedProcessedData: Shared): Attributes =
      others.foldLeft(primary.requestAttributes(request, sharedProcessedData))(
        _ ++ _.requestAttributes(request)
      )
    def responseAttributes[F[_]](response: Response[F]): Attributes =
      others.foldLeft(primary.responseAttributes(response))(_ ++ _.responseAttributes(response))
    def exceptionAttributes(cause: Throwable): Attributes =
      others.foldLeft(primary.exceptionAttributes(cause))(_ ++ _.exceptionAttributes(cause))

    override def and(that: AttributeProvider): SpanDataProvider = that match {
      case AttributeProvider.Empty => this
      case AttributeProvider.Multi(providers) => copy(others = others ++ providers)
      case _ => copy(others = others :+ that)
    }
  }
}
