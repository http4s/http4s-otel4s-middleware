package example

import cats._
import cats.effect.{ Trace => _, _ }
import cats.syntax.all._
import io.jaegertracing.Configuration.ReporterConfiguration
import io.jaegertracing.Configuration.SamplerConfiguration
import natchez._
import natchez.jaeger.Jaeger
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpRoutes
import org.http4s.client.Client
import org.http4s._
import org.http4s.implicits._

trait Common {

  // A dumb subroutine that does some tracing
  def greet[F[_]: Monad: Trace](input: String) =
    Trace[F].span("greet") {
      for {
        _ <- Trace[F].put("input" -> input)
      } yield s"Hello $input!\n"
    }

  // Our routes, in abstract F with a Trace constraint.
  def routes[F[_]: Trace](client: Client[F])(
    implicit ev: MonadError[F, Throwable]
  ): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F]; import dsl._ // bleh
    HttpRoutes.of[F] {

      case GET -> Root / "hello" / name =>
        for {
          str <- greet[F](name)
          res <- Ok(str)
        } yield res
      case GET -> Root / "client" / "hello" / name => 
        client.toHttpApp.run(Request(Method.GET, uri"http://localhost:8080/hello" / name))
      case GET -> Root / "fail" =>
        ev.raiseError(new RuntimeException("💥 Boom!"))

    }
  }

  // A Jaeger entry point
  def entryPoint[F[_]: Sync]: Resource[F, EntryPoint[F]] =
    Jaeger.entryPoint[F](
      system    = "Http4sExample",
      uriPrefix = Some(new java.net.URI("http://localhost:16686")),
    ) { c =>
      Sync[F].delay {
        c.withSampler(SamplerConfiguration.fromEnv)
         .withReporter(ReporterConfiguration.fromEnv)
         .getTracer
      }
    }
}