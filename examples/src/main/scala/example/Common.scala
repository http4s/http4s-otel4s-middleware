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

package example

import cats._
import cats.effect.{Trace => _, _}
import cats.syntax.all._
import org.http4s._
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.otel4s.middleware.client.UriTemplateClassifier
import org.http4s.otel4s.middleware.server.RouteClassifier
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer

trait Common {

  // A trivial subroutine that does some tracing
  def greet[F[_]: Monad: Tracer](input: String): F[String] =
    Tracer[F].span("greet").use { span =>
      for {
        _ <- span.addAttribute(Attribute("input", input))
      } yield s"Hello $input!\n"
    }

  // Our routes, in abstract F with a Tracer constraint.
  def routes[F[_]: Tracer: Concurrent](client: Client[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          str <- greet[F](name)
          res <- Ok(str)
        } yield res
      case GET -> Root / "client" / "hello" / name =>
        client
          .expect[String](Request[F](Method.GET, uri"http://localhost:8080/hello" / name))
          .flatMap(Ok(_))
      case GET -> Root / "client" / "proxy" / "hello" / name =>
        client.toHttpApp.run(Request[F](Method.GET, uri"http://localhost:8080/client/hello" / name))
      case GET -> Root / "fail" =>
        Concurrent[F].raiseError(new RuntimeException("💥 Boom!"))
    }
  }

  def routeClassifier[F[_]]: RouteClassifier = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    RouteClassifier.of[F] {
      case GET -> Root / "hello" / _ =>
        "/hello/{name}"
      case GET -> Root / "client" / "hello" / _ =>
        "/client/hello/{name}"
      case GET -> Root / "client" / "proxy" / "hello" / _ =>
        "/client/proxy/hello/{name}"
      case GET -> Root / "fail" =>
        "/fail"
    }
  }

  def urlTemplateClassifier: UriTemplateClassifier = {
    val clientSegment = Uri.Path.Segment("client")
    val failSegment = Uri.Path.Segment("fail")
    val helloSegment = Uri.Path.Segment("hello")
    val proxySegment = Uri.Path.Segment("proxy")

    uri =>
      uri.path.segments match {
        case Seq(`helloSegment`, _) => Some("/hello/{name}")
        case Seq(`clientSegment`, `helloSegment`, _) => Some("/client/hello/{name}")
        case Seq(`clientSegment`, `proxySegment`, `helloSegment`, _) =>
          Some("/client/proxy/hello/{name}")
        case Seq(`failSegment`) => Some("/fail")
        case _ => None
      }
  }
}
