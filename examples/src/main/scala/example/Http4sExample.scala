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

  private def localForIoLocal[F[_]: MonadCancelThrow: LiftIO, E](
      ioLocal: IOLocal[E]
  ): Local[F, E] =
    new Local[F, E] {
      def applicative =
        Applicative[F]
      def ask[E2 >: E] =
        Functor[F].widen[E, E2](ioLocal.get.to[F])
      def local[A](fa: F[A])(f: E => E): F[A] =
        MonadCancelThrow[F].bracket(ioLocal.modify(e => (f(e), e)).to[F])(_ =>
          fa
        )(ioLocal.set(_).to[F])
    }

  def globalOtel4s[F[_]: Async: LiftIO]: Resource[F, (Otel4s[F], Local[F, Vault])] =
    Resource
      .eval(Sync[F].delay(GlobalOpenTelemetry.get))
      .evalMap{ jOtel =>
        LiftIO[F].liftIO(IOLocal(Vault.empty))
          .map { (ioLocal: IOLocal[Vault]) =>
            val local = localForIoLocal[F, Vault](ioLocal)
            OtelJava.local[F](jOtel)(Async[F], local) -> local
          }
      }


  def tracer[F[_]](otel: Otel4s[F]): F[Tracer[F]] =
    otel.tracerProvider.tracer("Http4sExample").get

  def propagator[F[_]](otel: Otel4s[F]): TextMapPropagator[F] =
    otel.propagators.textMapPropagator


  // Our main app resource
  def server[F[_]: Async: Tracer: TextMapPropagator](getVault: F[Vault]): Resource[F, Server] =
    for {
      client <- EmberClientBuilder.default[F].build
        .map(ClientMiddleware.default(getVault).build)
      app = ServerMiddleware.default[F].buildHttpApp{
        routes(client).orNotFound
      }
      sv <- EmberServerBuilder.default[F].withPort(port"8080").withHttpApp(app).build
    } yield sv

  // Done!
  def run(args: List[String]): IO[ExitCode] =
    globalOtel4s[IO].flatMap{
      case (otel4s, local) =>
        Resource.eval(tracer(otel4s)).flatMap{ implicit T: Tracer[IO] =>
          implicit val P: TextMapPropagator[IO] = propagator(otel4s)
          server[IO](local.ask)
        }
    }.use(_ => IO.never)

}