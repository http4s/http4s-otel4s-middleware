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

import cats.effect._
import cats.effect.syntax.all._
import com.comcast.ip4s._
import fs2.io.net.Network
import org.http4s.Query
import org.http4s.Uri
import org.http4s.Uri.Fragment
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact
import org.http4s.otel4s.middleware.trace.redact.HeaderRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.server.Server
import org.http4s.server.middleware.Metrics
import org.typelevel.otel4s.Otel4s
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

/** Start up Jaeger thus:
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

  // you probably want to be a bit more permissive than this
  val redactor: UriRedactor = new UriRedactor {
    def redactPath(path: Uri.Path): Uri.Path =
      if (path.isEmpty) path
      else Uri.Path.Root / redact.REDACTED

    def redactQuery(query: Query): Query =
      if (query.isEmpty) query
      else Query(redact.REDACTED -> None)

    def redactFragment(fragment: Fragment): Option[Fragment] =
      Some(if (fragment.isEmpty) fragment else redact.REDACTED)
  }

  def tracer[F[_]](otel: Otel4s[F]): F[Tracer[F]] =
    otel.tracerProvider.tracer("Http4sExample").get

  // Our main app resource
  def server[F[_]: Async: Network: TracerProvider: Tracer: MeterProvider]: Resource[F, Server] =
    for {
      clientMiddleware <- ClientMiddleware
        .builder[F] {
          ClientSpanDataProvider
            .openTelemetry(redactor)
            .withUrlTemplateClassifier(urlTemplateClassifier)
            .optIntoHttpRequestHeaders(HeaderRedactor.default)
            .optIntoHttpResponseHeaders(HeaderRedactor.default)
            .optIntoUrlScheme
            .optIntoUrlTemplate
            .optIntoUserAgentOriginal
        }
        .build
        .toResource
      client <- EmberClientBuilder
        .default[F]
        .build
        .map(clientMiddleware.wrap)
      metricsOps <- OtelMetrics.serverMetricsOps[F]().toResource
      serverMiddleware <- ServerMiddleware
        .builder[F] {
          ServerSpanDataProvider
            .openTelemetry(redactor)
            .withRouteClassifier(routeClassifier)
            .optIntoClientPort
            .optIntoHttpRequestHeaders(HeaderRedactor.default)
            .optIntoHttpResponseHeaders(HeaderRedactor.default)
        }
        .build
        .toResource
      sv <- EmberServerBuilder
        .default[F]
        .withPort(port"8080")
        .withHttpApp {
          serverMiddleware.wrapHttpApp {
            Metrics(metricsOps)(routes(client)).orNotFound
          }
        }
        .build
    } yield sv

  // Done!
  def run(args: List[String]): IO[ExitCode] =
    OtelJava
      .autoConfigured[IO]()
      .flatMap { otel4s =>
        implicit val TP: TracerProvider[IO] = otel4s.tracerProvider
        implicit val MP: MeterProvider[IO] = otel4s.meterProvider
        Resource.eval(tracer(otel4s)).flatMap { implicit T: Tracer[IO] =>
          server[IO]
        }
      }
      .use(_ => IO.never)

}
