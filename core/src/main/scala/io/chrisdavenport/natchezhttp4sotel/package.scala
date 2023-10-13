package io.chrisdavenport

import org.http4s.{Header, Headers}
import org.typelevel.ci.CIString
import org.typelevel.otel4s.{TextMapGetter, TextMapUpdater}

package object natchezhttp4sotel {
  implicit val headersTMU: TextMapUpdater[Headers] =
    (carrier: Headers, key: String, value: String) => carrier.put(Header.Raw(CIString(key), value))
  implicit val headersTMG: TextMapGetter[Headers] =
    new TextMapGetter[Headers] {
      def get(carrier: Headers, key: String): Option[String] =
        carrier.get(CIString(key)).map(_.head.value)
      def keys(carrier: Headers): Iterable[String] =
        carrier.headers.view.map(_.name).distinct.map(_.toString).toSeq
    }
}
