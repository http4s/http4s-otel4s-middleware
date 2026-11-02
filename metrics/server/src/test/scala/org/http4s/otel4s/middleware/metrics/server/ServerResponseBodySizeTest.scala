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

package org.http4s.otel4s.middleware.metrics.server

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import fs2.Chunk
import fs2.Stream
import fs2.compression.Compression
import fs2.io.compression._
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.HttpApp
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.otel4s.middleware.PerRequestFilter
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.server.middleware.GZip
import org.http4s.syntax.literals._
import org.typelevel.ci.CIStringSyntax
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
import org.typelevel.otel4s.semconv.attributes.UrlAttributes
import org.typelevel.otel4s.semconv.experimental.metrics.HttpExperimentalMetrics

class ServerResponseBodySizeTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val spec = HttpExperimentalMetrics.ServerResponseBodySize
  private val requestSpec = HttpExperimentalMetrics.ServerRequestBodySize
  private val buckets = BucketBoundaries(Vector(0d, 3d, 7d))
  private val payload = Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO]
  private val request = Request[IO](uri = uri"https://example.test/items")
  List(
    ("declared full", BodySizeConfig.contentLength(buckets), Some(99L), "full", Some(99L)),
    ("declared missing", BodySizeConfig.contentLength(buckets), None, "full", None),
    ("declared zero", BodySizeConfig.contentLength(buckets), Some(0L), "full", Some(0L)),
    ("observed mismatch", BodySizeConfig.observe(buckets), Some(99L), "full", Some(5L)),
    ("observed full", BodySizeConfig.observe(buckets), None, "full", Some(5L)),
    ("observed empty", BodySizeConfig.observe(buckets), None, "empty", Some(0L)),
    ("observed partial replay", BodySizeConfig.observe(buckets), None, "partial-replay", None),
  ).foreach { case (name, settings, length, consume, expected) =>
    test(name) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          effects <- Ref.of[IO, Int](0)
          middleware <- ServerMetrics.middleware[IO](kit, config(settings))
          source =
            (if (consume == "empty") Stream.empty else payload).onFinalize(effects.update(_ + 1))
          response <- middleware(
            HttpApp[IO](_ => IO.pure(declared(Response[IO]().withBodyStream(source), length)))
          )(request)
          before <- kit.collectMetrics
          _ = absent(before)
          _ <- consume match {
            case "partial-replay" =>
              response.body.take(1).compile.drain >> response.body.compile.drain
            case _ => response.body.compile.drain
          }
          metrics <- kit.collectMetrics
          count <- effects.get
        } yield {
          check(metrics, expected, terminal())
          assertEquals(count, if (consume.endsWith("replay")) 2 else 1)
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
    val settings =
      if (observed) BodySizeConfig.observe(buckets) else BodySizeConfig.contentLength(buckets)
    test(
      s"protocol-known zero preserves body effects and data: ${settings.strategy}/$method/${status.code}"
    ) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          effects <- Ref.of[IO, Int](0)
          middleware <- ServerMetrics.middleware[IO](kit, config(settings))
          response <- middleware(
            HttpApp[IO](_ =>
              IO.pure(
                declared(
                  Response[IO](status)
                    .withBodyStream(payload.onFinalize(effects.update(_ + 1))),
                  Some(99L),
                )
              )
            )
          )(request.withMethod(method))
          bytes <- response.body.compile.toVector
          metrics <- kit.collectMetrics
          count <- effects.get
        } yield {
          assertEquals(bytes, Vector[Byte](1, 2, 3, 4, 5))
          assertEquals(count, 1)
          check(metrics, Some(0L), terminal(status, method), method)
        }
      }
    }
  }
  List(
    (false, Method.GET, Status.Ok),
    (true, Method.GET, Status.Ok),
    (true, Method.HEAD, Status.Ok),
    (true, Method.GET, Status.NoContent),
    (false, Method.CONNECT, Status.Ok),
  ).foreach { case (observed, method, status) =>
    val settings =
      if (observed) BodySizeConfig.observe(buckets) else BodySizeConfig.contentLength(buckets)
    test(
      s"an unstarted response has no sample, even with a known size: ${settings.strategy}/$method/${status.code}"
    ) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config(settings))
          _ <- middleware(HttpApp[IO](_ => IO.pure(declared(Response[IO](status), Some(99L)))))(
            request.withMethod(method)
          )
          metrics <- kit.collectMetrics
        } yield {
          absent(metrics)
          assertEquals(
            MetricExpectations
              .find(metrics, MetricExpectation.name("http.server.request.duration")),
            None,
          )
          assertMetrics(
            metrics,
            MetricExpectation
              .sum[Long]("http.server.active_requests")
              .exactlyPoints(PointExpectation.numeric(1L).attributesExact(start(method))),
          )
        }
      }
    }
  }
  List(BodySizeConfig.contentLength(buckets), BodySizeConfig.observe(buckets)).foreach { settings =>
    test(s"unsuccessful CONNECT still has an HTTP body: ${settings.strategy}") {
      val status = Status.Forbidden
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config(settings))
          response <- middleware(
            HttpApp[IO](_ =>
              IO.pure(declared(Response[IO](status).withBodyStream(payload), Some(99L)))
            )
          )(request.withMethod(Method.CONNECT))
          _ <- response.body.compile.drain
          metrics <- kit.collectMetrics
        } yield check(
          metrics,
          Some(if (settings.strategy == BodySizeConfig.Strategy.Observe) 5L else 99L),
          terminal(status, Method.CONNECT),
          Method.CONNECT,
        )
      }
    }
    List(false, true).foreach { cleanup =>
      test(
        s"source/cleanup failure preserves the original error and only a known size: ${settings.strategy}/cleanup=$cleanup"
      ) {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val boom = new IllegalStateException("response")
          for {
            middleware <- ServerMetrics.middleware[IO](kit, config(settings))
            source =
              if (cleanup) payload.onFinalize(IO.raiseError[Unit](boom))
              else payload ++ Stream.raiseError[IO](boom)
            response <- middleware(
              HttpApp[IO](_ =>
                IO.pure(
                  declared(
                    Response[IO](Status.ServiceUnavailable)
                      .withBodyStream(source),
                    Some(99L),
                  )
                )
              )
            )(request)
            result <- response.body.compile.drain.attempt
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(result, Left(boom))
            check(
              metrics,
              Option.when(settings.strategy == BodySizeConfig.Strategy.ContentLength)(99L),
              terminal(Status.ServiceUnavailable, error = Some(boom)),
            )
          }
        }
      }
    }
    test(
      s"response cancellation retains error-status classification and a declared size: ${settings.strategy}"
    ) {
      val status = Status.ServiceUnavailable
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          for {
            entered <- Deferred[IO, Unit]
            middleware <- ServerMetrics.middleware[IO](kit, config(settings))
            response <- middleware(
              HttpApp[IO](_ =>
                IO.pure(
                  declared(
                    Response[IO](status).withBodyStream(
                      payload ++ Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
                    ),
                    Some(99L),
                  )
                )
              )
            )(request)
            fiber <- response.body.compile.drain.start
            _ <- entered.get >> fiber.cancel
            metrics <- kit.collectMetrics
          } yield check(
            metrics,
            Option.when(settings.strategy == BodySizeConfig.Strategy.ContentLength)(99L),
            terminal(status),
          )
        }
      }
    }
    List(false, true).foreach { cancel =>
      test(
        s"an app that returns no response has no response-size sample: ${settings.strategy}/cancel=$cancel"
      ) {
        TestControl.executeEmbed {
          OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
            val boom = new IllegalStateException("app")
            for {
              entered <- Deferred[IO, Unit]
              middleware <- ServerMetrics.middleware[IO](kit, config(settings))
              app = middleware(
                HttpApp[IO](_ =>
                  if (cancel) entered.complete(()).void >> IO.never[Response[IO]]
                  else IO.raiseError[Response[IO]](boom)
                )
              )
              _ <-
                if (cancel) for {
                  fiber <- app(request).start
                  _ <- entered.get >> fiber.cancel
                } yield ()
                else app(request).attempt.flatMap(r => IO(assertEquals(r, Left(boom))))
              metrics <- kit.collectMetrics
            } yield check(
              metrics,
              None,
              start() ++ Attributes(NetworkAttributes.NetworkProtocolVersion("1.1")) ++
                Option.when(!cancel)(ErrorAttributes.ErrorType(boom.getClass.getName)),
            )
          }
        }
      }
    }

  }

  List(false, true).foreach { afterEof =>
    test(s"consumer failures outside the observer do not invent HTTP errors: afterEof=$afterEof") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        val boom = new IllegalStateException("consumer")
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
          response <- middleware(HttpApp[IO](_ => IO.pure(Response[IO]().withBodyStream(payload))))(
            request
          )
          result <- (if (afterEof) response.body.compile.drain >> IO.raiseError[Unit](boom)
                     else
                       response.body.evalMap(_ => IO.raiseError[Byte](boom)).compile.drain).attempt
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          check(metrics, Option.when(afterEof)(5L), terminal())
        }
      }
    }
  }
  test("catching a response-source error keeps its HTTP error and unknown size") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val boom = new IllegalStateException("source")
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        response <- middleware(
          HttpApp[IO](_ =>
            IO.pure(Response[IO]().withBodyStream(payload ++ Stream.raiseError[IO](boom)))
          )
        )(request)
        _ <- response.body.handleErrorWith(_ => Stream.empty).compile.drain
        metrics <- kit.collectMetrics
      } yield check(metrics, None, terminal(error = Some(boom)))
    }
  }
  test("observation preserves demand and chunk boundaries") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val first = Chunk.array(Array[Byte](1, 2, 3))
      val second = Chunk.array(Array[Byte](4, 5))
      for {
        effects <- Ref.of[IO, Int](0)
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        source = Stream.eval(effects.update(_ + 1)).drain ++ Stream.chunk(first) ++
          Stream.eval(effects.update(_ + 1)).drain ++ Stream.chunk(second)
        response <- middleware(HttpApp[IO](_ => IO.pure(Response[IO]().withBodyStream(source))))(
          request
        )
        before <- effects.get
        chunks <- response.body.chunks.compile.toVector
        after <- effects.get
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(before, 0)
        assertEquals(chunks, Vector(first, second))
        assertEquals(after, 2)
        check(metrics, Some(5L), terminal())
      }
    }
  }
  test("request and response sources and buckets are independent") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val requestBuckets = BucketBoundaries(Vector(0d, 2d, 10d))
      for {
        middleware <- ServerMetrics.middleware[IO](
          kit,
          config(BodySizeConfig.observe(buckets))
            .optIntoRequestBodySize(BodySizeConfig.contentLength(requestBuckets)),
        )
        response <- middleware(HttpApp[IO](_ => IO.pure(Response[IO]().withBodyStream(payload))))(
          request.putHeaders(Header.Raw(ci"Content-Length", "17"))
        )
        _ <- response.body.compile.drain
        metrics <- kit.collectMetrics
      } yield {
        check(metrics, Some(5L), terminal())
        assertMetrics(
          metrics,
          MetricExpectation
            .histogram(requestSpec.name)
            .unit(requestSpec.unit)
            .exactlyPoints(
              PointExpectation.histogram
                .count(1L)
                .sum(17d)
                .boundaries(requestBuckets)
                .attributesExact(terminal())
            ),
        )
      }
    }
  }
  List(false, true).foreach { cancel =>
    test(s"completed request size survives response failure/cancellation: cancel=$cancel") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val boom = new IllegalStateException("response")
          for {
            entered <- Deferred[IO, Unit]
            middleware <- ServerMetrics.middleware[IO](
              kit,
              config(BodySizeConfig.observe(buckets))
                .optIntoRequestBodySize(BodySizeConfig.observe(buckets)),
            )
            source =
              if (cancel) Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
              else Stream.raiseError[IO](boom)
            response <- middleware(
              HttpApp[IO](req => req.body.compile.drain.as(Response[IO]().withBodyStream(source)))
            )(request.withBodyStream(payload))
            _ <-
              if (cancel) for {
                fiber <- response.body.compile.drain.start
                _ <- entered.get >> fiber.cancel
              } yield ()
              else response.body.compile.drain.attempt.flatMap(r => IO(assertEquals(r, Left(boom))))
            metrics <- kit.collectMetrics
          } yield {
            val labels = terminal(error = Option.when(!cancel)(boom))
            check(metrics, None, labels)
            assertMetrics(
              metrics,
              MetricExpectation
                .histogram(requestSpec.name)
                .exactlyPoints(PointExpectation.histogram.count(1L).sum(5d).attributesExact(labels)),
            )
          }
        }
      }
    }
  }
  test("disabled and excluded response-size instrumentation emits no sample") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val original = Response[IO]().withBodyStream(payload)
      for {
        disabled <- ServerMetrics.middleware[IO](kit, ServerMetricsConfig.default)
        first <- disabled(HttpApp[IO](_ => IO.pure(original)))(request)
        _ <- first.body.compile.drain
        excluded <- ServerMetrics.middleware[IO](
          kit,
          config(BodySizeConfig.observe(buckets))
            .withRequestFilter(PerRequestFilter.neverEnabled),
        )
        second <- excluded(HttpApp[IO](_ => IO.pure(original)))(request)
        _ = assert(second eq original)
        _ <- second.body.compile.drain
        metrics <- kit.collectMetrics
      } yield absent(metrics)
    }
  }
  test("each invocation has independent state; response replay cannot add a sample") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        effect = middleware(HttpApp[IO](_ => IO.pure(Response[IO]().withBodyStream(payload))))(
          request
        )
        first <- effect
        second <- effect
        _ <- first.body.compile.drain >> second.body.compile.drain >> first.body.compile.drain
        metrics <- kit.collectMetrics
      } yield assertMetrics(
        metrics,
        MetricExpectation
          .histogram(spec.name)
          .exactlyPoints(
            PointExpectation.histogram
              .count(2L)
              .sum(10d)
              .boundaries(buckets)
              .attributesExact(terminal())
          ),
      )
    }
  }

  List(false, true).foreach { observed =>
    List(false, true).foreach { outsideCompression =>
      test(
        s"GZip placement determines response representation: observe=$observed outsideCompression=$outsideCompression"
      ) {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val raw = Vector.fill[Byte](128)(42)
          val settings =
            if (observed) BodySizeConfig.observe(buckets) else BodySizeConfig.contentLength(buckets)
          for {
            middleware <- ServerMetrics.middleware[IO](kit, config(settings))
            base = HttpApp[IO](_ =>
              IO.pure(
                declared(Response[IO]().withBodyStream(Stream.emits(raw).covary[IO]), Some(128L))
              )
            )
            app = if (outsideCompression) middleware(GZip(base)) else GZip(middleware(base))
            response <- app(request.putHeaders(Header.Raw(ci"Accept-Encoding", "gzip")))
            encoded <- response.body.compile.toVector
            decoded <- Stream
              .emits(encoded)
              .covary[IO]
              .through(Compression[IO].gunzip())
              .flatMap(_.content)
              .compile
              .toVector
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(decoded, raw)
            assertNotEquals(encoded.size, raw.size)
            val expected =
              if (!outsideCompression) Some(128L)
              else if (observed) Some(encoded.size.toLong)
              else None
            check(metrics, expected, terminal())
          }
        }
      }
    }
  }

  private def start(method: Method = Method.GET): Attributes = Attributes(
    HttpAttributes.HttpRequestMethod(method.name),
    UrlAttributes.UrlScheme("https"),
    Attribute("component", "server"),
  )

  private def terminal(
      status: Status = Status.Ok,
      method: Method = Method.GET,
      error: Option[Throwable] = None,
  ): Attributes = {
    val labels = start(method) ++ Attributes(
      NetworkAttributes.NetworkProtocolVersion("1.1"),
      HttpAttributes.HttpResponseStatusCode(status.code.toLong),
    )
    labels ++ error.map(e => ErrorAttributes.ErrorType(e.getClass.getName)) ++
      Option.when(error.isEmpty && status.code >= 500)(
        ErrorAttributes.ErrorType(status.code.toString)
      )
  }

  private def config(size: BodySizeConfig): ServerMetricsConfig =
    ServerMetricsConfig.default.optIntoActiveRequests
      .withAttributes(Attributes(Attribute("component", "server")))
      .optIntoResponseBodySize(size)

  private def declared(response: Response[IO], length: Option[Long]): Response[IO] =
    length.fold(response)(n => response.putHeaders(Header.Raw(ci"Content-Length", n.toString)))

  private def absent(metrics: List[MetricData])(implicit loc: Location): Unit =
    assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(spec.name)), None)

  private def size(value: Long, labels: Attributes, bounds: BucketBoundaries): MetricExpectation =
    MetricExpectation
      .histogram(spec.name)
      .unit(spec.unit)
      .description(spec.description)
      .exactlyPoints(
        PointExpectation.histogram
          .count(1L)
          .sum(value.toDouble)
          .boundaries(bounds)
          .attributesExact(labels)
      )

  private def check(
      metrics: List[MetricData],
      expected: Option[Long],
      labels: Attributes,
      method: Method = Method.GET,
      bounds: BucketBoundaries = buckets,
  )(implicit loc: Location): Unit = {
    val expectations =
      List(activeMetric(0L, start(method)), durationMetric(labels)) ++ expected.map(
        size(_, labels, bounds)
      )
    assertMetrics(metrics, expectations: _*)
    if (expected.isEmpty) absent(metrics)
  }

}
