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
import org.http4s.HttpRoutes
import org.http4s._
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer

trait Common {

  // A dumb subroutine that does some tracing
  def greet[F[_]: Monad: Tracer](input: String): F[String] =
    Tracer[F].span("greet").use { span =>
      for {
        _ <- span.addAttribute(Attribute("input", input))
      } yield s"Hello $input!\n"
    }

  // Our routes, in abstract F with a Trace constraint.
  def routes[F[_]: Tracer: Concurrent](client: Client[F]): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F]; import dsl._ // bleh
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

}
