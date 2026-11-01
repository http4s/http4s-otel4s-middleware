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

package org.http4s.otel4s.middleware.metrics.client

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.functor._
import fs2.Stream
import fs2.compression.Compression
import fs2.io.compression._
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.client.middleware.GZip
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.syntax.literals._
import org.typelevel.ci.CIString
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics

class ClientResponseBodySizeTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val sizeName = HttpExperimentalMetrics.ClientResponseBodySize.name
  private val requestSizeName = HttpExperimentalMetrics.ClientRequestBodySize.name
  private val buckets = BucketBoundaries(Vector(0d, 3d, 7d))
  private val payload = Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO]
  private val request = Request[IO](Method.GET, uri"https://example.test/items")

  List(
    ("declared unread", BodySizeConfig.contentLength(buckets), Some(99L), "unread", Some(99L)),
    ("declared missing", BodySizeConfig.contentLength(buckets), None, "full", None),
    ("declared zero", BodySizeConfig.contentLength(buckets), Some(0L), "unread", Some(0L)),
    ("observed mismatch", BodySizeConfig.observe(buckets), Some(99L), "full", Some(5L)),
    ("observed full", BodySizeConfig.observe(buckets), None, "full", Some(5L)),
    ("observed empty", BodySizeConfig.observe(buckets), None, "empty", Some(0L)),
    ("observed unread", BodySizeConfig.observe(buckets), Some(99L), "unread", None),
    ("observed partial replay", BodySizeConfig.observe(buckets), None, "partial-replay", None),
  ).foreach { case (name, sizeConfig, declared, consume, expected) =>
    test(s"body size: $name") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](kit, config(sizeConfig))
          effects <- Ref.of[IO, Int](0)
          source =
            (if (consume == "empty") Stream.empty else payload).onFinalize(effects.update(_ + 1))
          response = length(Response[IO]().withBodyStream(source), declared)
          _ <- middleware(Client[IO](_ => Resource.pure(response))).run(request).use { received =>
            consume match {
              case "unread" => IO.unit
              case "partial-replay" =>
                received.body.take(1).compile.drain >> received.body.compile.drain
              case _ => received.body.compile.drain
            }
          }
          metrics <- kit.collectMetrics
          evaluations <- effects.get
        } yield {
          check(metrics, expected, terminal())
          assertEquals(
            evaluations,
            if (consume == "unread") 0 else if (consume.endsWith("replay")) 2 else 1,
          )
          assertEquals(
            MetricExpectations.find(metrics, MetricExpectation.name(requestSizeName)),
            None,
          )
        }
      }
    }
  }

  List(
    (false, Method.HEAD, Status.Ok),
    (true, Method.HEAD, Status.Ok),
    (false, Method.GET, Status.NoContent),
    (true, Method.GET, Status.ResetContent),
    (false, Method.GET, Status.NotModified),
    (true, Method.GET, Status.fromInt(199).toOption.get),
    (true, Method.CONNECT, Status.Created),
  ).foreach { case (observed, method, status) =>
    test(s"protocol-known zero overrides declared length: observe=$observed $method $status") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](
            kit,
            config(
              if (observed) BodySizeConfig.observe(buckets)
              else BodySizeConfig.contentLength(buckets)
            ),
          )
          response = length(Response[IO](status), Some(99L))
          _ <- middleware(Client[IO](_ => Resource.pure(response)))
            .run(request.withMethod(method))
            .use(_ => IO.unit)
          metrics <- kit.collectMetrics
        } yield check(metrics, Some(0L), terminal(status, method), method)
      }
    }
  }

  test("an unsuccessful CONNECT can still have an observed HTTP body") {
    val status = Status.Forbidden
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        _ <- middleware(
          Client[IO](_ => Resource.pure(Response[IO](status).withBodyStream(payload)))
        )
          .run(request.withMethod(Method.CONNECT))
          .use(_.body.compile.drain)
        metrics <- kit.collectMetrics
      } yield check(metrics, Some(5L), terminal(status, Method.CONNECT), Method.CONNECT)
    }
  }

  test("protocol-zero handling preserves body effects rather than changing the response stream") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        effects <- Ref.of[IO, Int](0)
        response = Response[IO]().withBodyStream(payload.onFinalize(effects.update(_ + 1)))
        bytes <- middleware(Client[IO](_ => Resource.pure(response)))
          .run(request.withMethod(Method.HEAD))
          .use(_.body.compile.toVector)
        count <- effects.get
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(bytes, Vector[Byte](1, 2, 3, 4, 5))
        assertEquals(count, 1)
        check(metrics, Some(0L), terminal(method = Method.HEAD), Method.HEAD)
      }
    }
  }

  List(false, true).foreach { observed =>
    List(false, true).foreach { cleanup =>
      test(
        s"body failure keeps declared size and omits incomplete observation: observe=$observed cleanup=$cleanup"
      ) {
        val boom = new RuntimeException("response body failure")
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          for {
            middleware <- ClientMetrics.middleware[IO](
              kit,
              config(
                if (observed) BodySizeConfig.observe(buckets)
                else BodySizeConfig.contentLength(buckets)
              ),
            )
            source =
              if (cleanup) payload.onFinalize(IO.raiseError[Unit](boom))
              else payload ++ Stream.raiseError[IO](boom)
            response = length(Response[IO]().withBodyStream(source), Some(99L))
            result <- middleware(Client[IO](_ => Resource.pure(response)))
              .run(request)
              .use(_.body.compile.drain)
              .attempt
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(result, Left(boom))
            check(metrics, if (observed) None else Some(99L), terminal(error = Some(boom)))
          }
        }
      }
    }
  }

  List(false, true).foreach { observed =>
    test(s"acquisition failure has no response-size sample: observe=$observed") {
      val boom = new RuntimeException("acquisition failure")
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](
            kit,
            config(
              if (observed) BodySizeConfig.observe(buckets)
              else BodySizeConfig.contentLength(buckets)
            ),
          )
          result <- middleware(Client[IO](_ => Resource.eval(IO.raiseError[Response[IO]](boom))))
            .run(request)
            .use(_ => IO.unit)
            .attempt
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          check(metrics, None, start().added(ErrorAttributes.ErrorType(boom.getClass.getName)))
        }
      }
    }
  }

  List(false, true).foreach { observed =>
    test(
      s"cancellation retains a completed request size and freezes response size: observe=$observed"
    ) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](
            kit,
            config(
              if (observed) BodySizeConfig.observe(buckets)
              else BodySizeConfig.contentLength(buckets)
            )
              .optIntoRequestBodySize(BodySizeConfig.observe(buckets)),
          )
          entered <- Deferred[IO, Unit]
          released <- Ref.of[IO, Int](0)
          source = (payload ++ Stream.exec(entered.complete(()).void) ++ Stream.never[IO])
            .onFinalize(released.update(_ + 1))
          response = length(Response[IO]().withBodyStream(source), Some(99L))
          base = Client[IO](req => Resource.eval(req.body.compile.drain).as(response))
          fiber <- middleware(base)
            .run(request.withBodyStream(payload))
            .use(_.body.compile.drain)
            .start
          _ <- entered.get
          _ <- fiber.cancel
          outcome <- fiber.join
          count <- released.get
          metrics <- kit.collectMetrics
        } yield {
          assert(outcome.isCanceled)
          assertEquals(count, 1)
          check(metrics, if (observed) None else Some(99L), terminal())
          assertMetrics(metrics, sizeExpectation(5L, terminal(), name = requestSizeName))
        }
      }
    }
  }

  test("a completed request remains measurable when the response body fails") {
    val boom = new RuntimeException("response failure")
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](
          kit,
          config(BodySizeConfig.observe(buckets))
            .optIntoRequestBodySize(BodySizeConfig.observe(buckets)),
        )
        base = Client[IO](req =>
          Resource
            .eval(req.body.compile.drain)
            .as(Response[IO]().withBodyStream(payload ++ Stream.raiseError[IO](boom)))
        )
        result <- middleware(base)
          .run(request.withBodyStream(payload))
          .use(_.body.compile.drain)
          .attempt
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(result, Left(boom))
        check(metrics, None, terminal(error = Some(boom)))
        assertMetrics(
          metrics,
          sizeExpectation(5L, terminal(error = Some(boom)), name = requestSizeName),
        )
      }
    }
  }

  List(false, true).foreach { afterEof =>
    test(
      s"consumer failure outside the observer preserves HTTP classification: afterEOF=$afterEof"
    ) {
      val boom = new RuntimeException("consumer failure")
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
          base = Client[IO](_ => Resource.pure(Response[IO]().withBodyStream(payload)))
          result <- middleware(base)
            .run(request)
            .use { response =>
              if (afterEof) response.body.compile.drain >> IO.raiseError[Unit](boom)
              else response.body.evalMap(_ => IO.raiseError[Byte](boom)).compile.drain
            }
            .attempt
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          check(metrics, if (afterEof) Some(5L) else None, terminal())
        }
      }
    }
  }

  test("catching a source error does not make its size complete or erase the HTTP error") {
    val boom = new RuntimeException("caught source error")
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        base = Client[IO](_ =>
          Resource.pure(Response[IO]().withBodyStream(payload ++ Stream.raiseError[IO](boom)))
        )
        _ <- middleware(base)
          .run(request)
          .use(_.body.handleErrorWith(_ => Stream.empty).compile.drain)
        metrics <- kit.collectMetrics
      } yield check(metrics, None, terminal(error = Some(boom)))
    }
  }

  test("request and response size sources and buckets are independent") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val requestBuckets = BucketBoundaries(Vector(0d, 2d, 10d))
      for {
        middleware <- ClientMetrics.middleware[IO](
          kit,
          config(BodySizeConfig.observe(buckets))
            .optIntoRequestBodySize(BodySizeConfig.contentLength(requestBuckets)),
        )
        req = request
          .withBodyStream(payload)
          .putHeaders(Header.Raw(CIString("Content-Length"), "17"))
        base = Client[IO](received =>
          Resource
            .eval(received.body.compile.drain)
            .as(Response[IO]().withBodyStream(payload))
        )
        _ <- middleware(base).run(req).use(_.body.compile.drain)
        metrics <- kit.collectMetrics
      } yield {
        check(metrics, Some(5L), terminal())
        assertMetrics(
          metrics,
          sizeExpectation(17L, terminal(), requestBuckets, name = requestSizeName),
        )
      }
    }
  }

  test("backend cleanup failure after EOF does not change the recorded size or HTTP outcome") {
    val boom = new RuntimeException("backend cleanup failure")
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        base = Client[IO](_ =>
          Resource
            .make(IO.pure(Response[IO]().withBodyStream(payload)))(_ => IO.raiseError[Unit](boom))
        )
        result <- middleware(base).run(request).use(_.body.compile.drain).attempt
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(result, Left(boom))
        check(metrics, Some(5L), terminal())
      }
    }
  }

  test("backend draining an unread response cannot add a late observed size") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        effects <- Ref.of[IO, Int](0)
        response = Response[IO]().withBodyStream(payload.onFinalize(effects.update(_ + 1)))
        base = Client[IO](_ => Resource.make(IO.pure(response))(_.body.compile.drain))
        _ <- middleware(base).run(request).use(_ => IO.unit)
        count <- effects.get
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(count, 1)
        check(metrics, None, terminal())
      }
    }
  }

  test("disabled and excluded response-size instrumentation produces no sample") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        disabled <- ClientMetrics.middleware[IO](kit, ClientMetricsConfig.default)
        excluded <- ClientMetrics.middleware[IO](
          kit,
          config(BodySizeConfig.observe(buckets))
            .withRequestFilter(PerRequestFilter.neverEnabled),
        )
        response = Response[IO]().withBodyStream(payload)
        base = Client[IO](_ => Resource.pure(response))
        _ <- disabled(base).run(request).use(_.body.compile.drain)
        _ <- excluded(base).run(request).use { received =>
          assert(received eq response)
          received.body.compile.drain
        }
        metrics <- kit.collectMetrics
      } yield assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(sizeName)), None)
    }
  }

  test("each acquisition of a reused resource has independent response observation state") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        resource = middleware(
          Client[IO](_ => Resource.pure(Response[IO]().withBodyStream(payload)))
        ).run(request)
        _ <- resource.use(_.body.compile.drain)
        _ <- resource.use(_.body.compile.drain)
        metrics <- kit.collectMetrics
      } yield assertMetrics(metrics, sizeExpectation(10L, terminal(), count = 2L))
    }
  }

  List(false, true).foreach { observed =>
    List(false, true).foreach { beforeDecompression =>
      test(
        s"GZip placement controls visible response representation: observe=$observed beforeDecompression=$beforeDecompression"
      ) {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val raw = Vector.fill[Byte](128)(42)
          for {
            encoded <- Stream.emits(raw).covary[IO].through(Compression[IO].gzip()).compile.toVector
            middleware <- ClientMetrics.middleware[IO](
              kit,
              config(
                if (observed) BodySizeConfig.observe(buckets)
                else BodySizeConfig.contentLength(buckets)
              ),
            )
            response = length(
              Response[IO]().withBodyStream(Stream.emits(encoded).covary[IO]),
              Some(encoded.size.toLong),
            )
              .putHeaders(Header.Raw(CIString("Content-Encoding"), "gzip"))
            base = Client[IO](_ => Resource.pure(response))
            client =
              if (beforeDecompression) GZip[IO]()(middleware(base))
              else middleware(GZip[IO]()(base))
            bytes <- client.run(request).use(_.body.compile.toVector)
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(bytes, raw)
            assertNotEquals(encoded.size, raw.size)
            val expected =
              if (beforeDecompression) Some(encoded.size.toLong)
              else if (observed) Some(raw.size.toLong)
              else None
            check(metrics, expected, terminal())
          }
        }
      }
    }
  }

  private def start(method: Method = Method.GET): Attributes = Attributes(
    HttpAttributes.HttpRequestMethod(method.name),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
    Attribute("component", "client"),
  )

  private def terminal(
      status: Status = Status.Ok,
      method: Method = Method.GET,
      error: Option[Throwable] = None,
  ): Attributes = {
    val base = start(method) ++ Attributes(
      HttpAttributes.HttpResponseStatusCode(status.code.toLong),
      NetworkAttributes.NetworkProtocolVersion("1.1"),
    )
    error.fold(
      if (status.code >= 400) base.added(ErrorAttributes.ErrorType(status.code.toString)) else base
    )(e => base.added(ErrorAttributes.ErrorType(e.getClass.getName)))
  }

  private def config(size: BodySizeConfig): ClientMetricsConfig =
    ClientMetricsConfig.default.optIntoActiveRequests
      .withAttributes(Attributes(Attribute("component", "client")))
      .optIntoResponseBodySize(size)

  private def length(response: Response[IO], size: Option[Long]): Response[IO] =
    size.fold(response)(n =>
      response.putHeaders(Header.Raw(CIString("Content-Length"), n.toString))
    )

  private def sizeExpectation(
      value: Long,
      labels: Attributes,
      bounds: BucketBoundaries = buckets,
      count: Long = 1L,
      name: String = sizeName,
  ): MetricExpectation =
    MetricExpectation
      .histogram(name)
      .unit("By")
      .exactlyPoints(
        PointExpectation.histogram
          .count(count)
          .sum(value.toDouble)
          .boundaries(bounds)
          .attributesExact(labels)
      )

  private def check(
      metrics: List[MetricData],
      size: Option[Long],
      labels: Attributes,
      method: Method = Method.GET,
      bounds: BucketBoundaries = buckets,
  )(implicit loc: Location): Unit = {
    val base = List(activeMetric(0L, start(method)), durationMetric(labels))
    assertMetrics(metrics, (base ++ size.map(sizeExpectation(_, labels, bounds))): _*)
    if (size.isEmpty)
      assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(sizeName)), None)
  }

}
