/*
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

package org.http4s.otel4s.middleware.metrics

import cats.effect.IO
import cats.effect.Resource
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.middleware.{Metrics => ClientMetrics}
import org.http4s.headers.`Content-Length`
import org.http4s.server.middleware.{Metrics => ServerMetrics}
import org.http4s.syntax.literals._
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.sdk.testkit.metrics._
import org.typelevel.otel4s.sdk.metrics.data.MetricData

import java.util.concurrent.TimeoutException

class OtelMetricsLifecycleTest extends CatsEffectSuite {
  private def kit[A](f: (MetricsTestkit[IO], MeterProvider[IO]) => IO[A]): IO[A] =
    MetricsTestkit.inMemory[IO]().use(k => f(k, k.meterProvider))

  private def counts(data: List[MetricData], name: String, n: Long): Unit =
    if (n == 0L) assert(!data.exists(_.name == name), s"unexpected $name: $data")
    else
      MetricExpectations
        .checkAllDistinct(
          data,
          MetricExpectation
            .histogram(name)
            .points(
              PointSetExpectation.exactly(PointExpectation.histogram.count(n))
            ),
        )
        .fold(ms => fail(MetricExpectations.format(ms)), identity)

  private def size(data: List[MetricData], n: Double): Unit =
    MetricExpectations
      .checkAllDistinct(
        data,
        MetricExpectation
          .histogram("http.client.response.body.size")
          .points(
            PointSetExpectation.exactly(PointExpectation.histogram.count(1L).sum(n))
          ),
      )
      .fold(ms => fail(MetricExpectations.format(ms)), identity)

  private def error(data: List[MetricData], value: String): Unit =
    MetricExpectations
      .checkAllDistinct(
        data,
        MetricExpectation
          .histogram("http.client.request.duration")
          .points(
            PointSetExpectation.exactly(
              PointExpectation.histogram
                .count(1L)
                .attributesSubset(ErrorAttributes.ErrorType(value))
            )
          ),
      )
      .fold(ms => fail(MetricExpectations.format(ms)), identity)

  private def clientResponse(body: Stream[IO, Byte], length: Option[Long]): Response[IO] = {
    val response = Response[IO](Status.Ok).withBodyStream(body)
    length.fold(response)(n => response.putHeaders(`Content-Length`(n)))
  }

  private def runClient(
      body: Stream[IO, Byte],
      length: Option[Long],
      consume: Response[IO] => IO[Unit],
  ): IO[List[MetricData]] = kit { (k, mp) =>
    implicit val meterProvider: MeterProvider[IO] = mp
    for {
      ops <- OtelMetrics.clientMetricsOps[IO](ClientMetricsConfig.all)
      client = ClientMetrics[IO](ops)(Client[IO](_ => Resource.pure(clientResponse(body, length))))
      _ <- client.run(Request[IO](Method.GET, uri"http://example.com/")).use(consume)
      data <- k.collectMetrics
    } yield data
  }

  private val bytes = Stream.emits("abcde".getBytes("UTF-8")).covary[IO]

  test("client full response consumption records body sizes with and without declared length") {
    for {
      declared <- runClient(bytes, Some(5), _.body.compile.drain)
      observed <- runClient(bytes, None, _.body.compile.drain)
    } yield {
      size(declared, 5.0)
      size(observed, 5.0)
    }
  }

  test(
    "client partial response consumption records unknown-length progress and skips declared size"
  ) {
    for {
      declared <- runClient(bytes, Some(5), _.body.take(2).compile.drain)
      observed <- runClient(
        Stream
          .emits("ab".getBytes("UTF-8"))
          .covary[IO] ++ Stream.emits("cde".getBytes("UTF-8")).covary[IO],
        None,
        _.body.take(2).compile.drain,
      )
    } yield {
      counts(declared, "http.client.response.body.size", 0L)
      size(observed, 2.0)
    }
  }

  test("unread client response records zero for unknown length and skips declared length") {
    for {
      declared <- runClient(bytes, Some(5), _ => IO.unit)
      observed <- runClient(bytes, None, _ => IO.unit)
    } yield {
      counts(declared, "http.client.response.body.size", 0L)
      size(observed, 0.0)
    }
  }

  test("caught client response body error is visible in duration termination attributes") {
    val boom = new RuntimeException("body failed")
    runClient(Stream.raiseError[IO](boom), None, _.body.compile.drain.attempt.void).map { data =>
      error(data, boom.getClass.getName)
    }
  }

  test("explicit client TimeoutException is recorded as a failed request") {
    kit { (k, mp) =>
      implicit val meterProvider: MeterProvider[IO] = mp
      for {
        ops <- OtelMetrics.clientMetricsOps[IO](ClientMetricsConfig.all)
        client = ClientMetrics[IO](ops)(
          Client[IO](_ => Resource.eval(IO.raiseError(new TimeoutException("timed out"))))
        )
        _ <- client.run(Request[IO](Method.GET, uri"http://example.com/")).use(_ => IO.unit).attempt
        data <- k.collectMetrics
      } yield error(data, "timeout")
    }
  }

  test("client response body cancellation releases active request") {
    TestControl.executeEmbed {
      kit { (k, mp) =>
        implicit val meterProvider: MeterProvider[IO] = mp
        for {
          ops <- OtelMetrics.clientMetricsOps[IO](ClientMetricsConfig.all)
          started <- IO.deferred[Unit]
          body = Stream.eval(started.complete(()).void >> IO.never[Byte])
          client = ClientMetrics[IO](ops)(
            Client[IO](_ => Resource.pure(clientResponse(body, None)))
          )
          fiber <- client
            .run(Request[IO](Method.GET, uri"http://example.com/"))
            .use(_.body.compile.drain)
            .start
          _ <- started.get
          _ <- fiber.cancel
          data <- k.collectMetrics
        } yield {
          error(data, "cancel")
          MetricExpectations
            .checkAllDistinct(
              data,
              MetricExpectation
                .sum[Long]("http.client.active_requests")
                .points(
                  PointSetExpectation.forall(PointExpectation.numeric(0L))
                ),
            )
            .fold(ms => fail(MetricExpectations.format(ms)), identity)
        }
      }
    }
  }

  test("server response replay completes metrics once") {
    kit { (k, mp) =>
      implicit val meterProvider: MeterProvider[IO] = mp
      for {
        ops <- OtelMetrics.serverMetricsOps[IO](ServerMetricsConfig.all)
        app = ServerMetrics[IO](ops)(
          HttpApp.pure[IO](clientResponse(Stream.emits("abc".getBytes("UTF-8")).covary[IO], None))
        )
        response <- app.run(Request[IO](Method.GET, uri"http://example.com/"))
        _ <- response.body.compile.drain
        _ <- response.body.compile.drain
        data <- k.collectMetrics
      } yield {
        counts(data, "http.server.request.duration", 1L)
        MetricExpectations
          .checkAllDistinct(
            data,
            MetricExpectation
              .sum[Long]("http.server.active_requests")
              .points(
                PointSetExpectation.forall(PointExpectation.numeric(0L))
              ),
          )
          .fold(ms => fail(MetricExpectations.format(ms)), identity)
      }
    }
  }
}
*/
