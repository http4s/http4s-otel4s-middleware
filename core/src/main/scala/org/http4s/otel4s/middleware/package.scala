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
package otel4s

import org.typelevel.ci.CIString
import org.typelevel.otel4s.context.propagation.TextMapGetter
import org.typelevel.otel4s.context.propagation.TextMapUpdater

package object middleware {
  implicit private[middleware] val headersTMG: TextMapGetter[Headers] =
    new TextMapGetter[Headers] {
      def get(carrier: Headers, key: String): Option[String] =
        carrier.get(CIString(key)).map(_.head.value)
      def keys(carrier: Headers): Iterable[String] =
        carrier.headers.view.map(_.name).distinct.map(_.toString).toSeq
    }
  implicit private[middleware] val headersTMU: TextMapUpdater[Headers] =
    (headers, key, value) => headers.put(Header.Raw(CIString(key), value))
}
