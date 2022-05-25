package example

import cats.effect._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.server.Server
import org.http4s.implicits._
import io.chrisdavenport.natchezhttp4sotel._
import io.chrisdavenport.fiberlocal.GenFiberLocal
import com.comcast.ip4s._

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

  // Our main app resource
  def server[F[_]: Async: GenFiberLocal]: Resource[F, Server] =
    for {
      iClient <- EmberClientBuilder.default[F].build
      ep <- entryPoint[F]
      app = ServerMiddleware.default(ep).buildHttpApp{implicit T: natchez.Trace[F] => 
        val client = ClientMiddleware.default(ep).build(iClient)
        routes(client).orNotFound
      }
      sv <- EmberServerBuilder.default[F].withPort(port"8080").withHttpApp(app).build
    } yield sv

  // Done!
  def run(args: List[String]): IO[ExitCode] =
    server[IO].use(_ => IO.never)

}