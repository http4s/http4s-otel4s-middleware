package example

import cats.effect._
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

  def globalOtel4s[F[_]: Async: LiftIO]: Resource[F, Otel4s[F]] =
    Resource
      .eval(Sync[F].delay(GlobalOpenTelemetry.get))
      .evalMap(OtelJava.forAsync[F])


  def tracer[F[_]: Async: LiftIO]: Resource[F, Tracer[F]] =
    globalOtel4s[F].evalMap(_.tracerProvider.tracer("Http4sExample").get)


  // Our main app resource
  def server[F[_]: Async: Tracer]: Resource[F, Server] =
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
    tracer[IO].flatMap{implicit T: Tracer[IO] => server[IO]}.use(_ => IO.never)

}