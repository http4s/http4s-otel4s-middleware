package example

import cats._
import cats.effect._
import cats.syntax.all._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.server.Server
import org.http4s.implicits._
import io.chrisdavenport.natchezhttp4sotel._
import com.comcast.ip4s._

import io.opentelemetry.api.GlobalOpenTelemetry
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.java.OtelJava
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.TextMapPropagator
import org.typelevel.vault.Vault
import cats.mtl.Local

/**
 * Start up Jaeger thus:
 *
 *  docker run -d --name jaeger \
 *    -e COLLECTOR_ZIPKIN_HTTP_PORT=9411 \
 *    -p 5775:5775/udp \
 *    -p 6831:6831/udp \
 *    -p 6832:6832/udp \
 *    -p 5778:5778 \
 *    -p 16686:16686 \
 *    -p 14268:14268 \
 *    -p 9411:9411 \
 *    jaegertracing/all-in-one:1.8
 *
 * Run this example and do some requests. Go to http://localhost:16686 and select `Http4sExample`
 * and search for traces.
*/
object Http4sExample extends IOApp with Common {

  def globalOtel4s[F[_]: Async: LiftIO]: F[(Otel4s[F], Local[F, Vault])] =
    Sync[F].delay(GlobalOpenTelemetry.get)
      .flatMap{ jOtel =>
        ExternalHelpers.localVault.map( local =>
          OtelJava.local[F](jOtel)(Async[F], local) -> local
        )
      }

  def tracer[F[_]](otel: Otel4s[F]): F[Tracer[F]] =
    otel.tracerProvider.tracer("Http4sExample").get

  def propagator[F[_]](otel: Otel4s[F]): TextMapPropagator[F] =
    otel.propagators.textMapPropagator


  // Our main app resource
  def server[F[_]: Async: Tracer: TextMapPropagator: ({type L[M[_]] = Local[M, Vault]})#L]: Resource[F, Server] =
    for {
      client <- EmberClientBuilder.default[F].build
        .map(ClientMiddleware.default.build)
      app = ServerMiddleware.default[F].buildHttpApp{
        routes(client).orNotFound
      }
      sv <- EmberServerBuilder.default[F].withPort(port"8080").withHttpApp(app).build
    } yield sv

  // Done!
  def run(args: List[String]): IO[ExitCode] =
    Resource.eval(globalOtel4s[IO]).flatMap{
      case (otel4s, local) =>
        implicit val L: Local[IO, Vault] = local
        Resource.eval(tracer(otel4s)).flatMap{ implicit T: Tracer[IO] =>
          implicit val P: TextMapPropagator[IO] = propagator(otel4s)
          server[IO]
        }
    }.use(_ => IO.never)

}