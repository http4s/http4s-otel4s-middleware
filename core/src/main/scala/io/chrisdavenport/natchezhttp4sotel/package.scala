package io.chrisdavenport

import cats.arrow.FunctionK
import cats.~>
import org.http4s.{Header, Headers}
import org.typelevel.ci.CIString
import org.typelevel.otel4s.{KindTransformer, TextMapGetter, TextMapUpdater}

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

  // TODO: remove after release of otel4s > 0.3.0-RC1
  implicit def identityKindTransformer[F[_]]: KindTransformer[F, F] =
    new KindTransformer[F, F] {
      val liftK: F ~> F = FunctionK.id
      def limitedMapK[A](ga: F[A])(f: F ~> F): F[A] = f(ga)
    }
}
