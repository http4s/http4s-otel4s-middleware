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
import cats.effect.Resource
import cats.effect.testkit.TestControl
import fs2.Stream
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

class ServerRequestBodySizeTest extends CatsEffectSuite with ServerMetricsTestSupport {
  private val spec = HttpExperimentalMetrics.ServerRequestBodySize
  private val buckets = BucketBoundaries(Vector(0d, 3d, 7d))
  private val payload = Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO]
  private val request =
    Request[IO](Method.POST, uri"https://example.test/items").withBodyStream(payload)
  private val start = Attributes(
    HttpAttributes.HttpRequestMethod("POST"),
    UrlAttributes.UrlScheme("https"),
    Attribute("component", "server"),
  )
  private val protocol = Attributes(NetworkAttributes.NetworkProtocolVersion("1.1"))
  private val terminal =
    start ++ protocol ++ Attributes(HttpAttributes.HttpResponseStatusCode(200L))
  List(
    ("declared unread", BodySizeConfig.contentLength(buckets), Some(99L), "unread", Some(99L)),
    ("declared missing", BodySizeConfig.contentLength(buckets), None, "full", None),
    ("declared zero", BodySizeConfig.contentLength(buckets), Some(0L), "unread", Some(0L)),
    ("observed mismatch", BodySizeConfig.observe(buckets), Some(99L), "full", Some(5L)),
    ("observed full", BodySizeConfig.observe(buckets), None, "full", Some(5L)),
    ("observed empty", BodySizeConfig.observe(buckets), None, "empty", Some(0L)),
    ("observed unread", BodySizeConfig.observe(buckets), Some(99L), "unread", None),
    ("observed partial replay", BodySizeConfig.observe(buckets), None, "partial-replay", None),
  ).foreach { case (name, settings, length, consume, expected) =>
    test(name) {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          evaluations <- Ref.of[IO, Int](0)
          middleware <- ServerMetrics.middleware[IO](kit, config(settings))
          source = (if (consume == "empty") Stream.empty else payload).onFinalize(
            evaluations.update(_ + 1)
          )
          req = declared(request.withBodyStream(source), length)
          app = middleware(HttpApp[IO] { received =>
            if (settings.strategy == BodySizeConfig.Strategy.ContentLength) assert(received eq req)
            val read = consume match {
              case "unread" => IO.unit
              case "partial-replay" =>
                received.body.take(1).compile.drain >> received.body.compile.drain
              case _ => received.body.compile.drain
            }
            read.as(Response[IO]())
          })
          response <- app(req)
          before <- kit.collectMetrics
          _ <- response.body.compile.drain
          metrics <- kit.collectMetrics
          effects <- evaluations.get
        } yield {
          absent(before)
          check(metrics, expected)
          assertEquals(
            effects,
            if (consume == "unread") 0 else if (consume.endsWith("replay")) 2 else 1,
          )
        }
      }
    }
  }

  List(BodySizeConfig.contentLength(buckets), BodySizeConfig.observe(buckets)).foreach { settings =>
    List("source", "cleanup", "app-after-eof", "response-after-eof").foreach { phase =>
      test(
        s"failure preserves error and records only a known request size: ${settings.strategy}/$phase"
      ) {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
          val boom = new IllegalStateException("failure")
          val complete = phase.endsWith("after-eof")
          val expected =
            if (settings.strategy == BodySizeConfig.Strategy.ContentLength) Some(99L)
            else Option.when(complete)(5L)
          for {
            middleware <- ServerMetrics.middleware[IO](kit, config(settings))
            source =
              if (phase == "source") payload ++ Stream.raiseError[IO](boom)
              else if (phase == "cleanup") payload.onFinalize(IO.raiseError[Unit](boom))
              else payload
            responseF = middleware(
              HttpApp[IO](req =>
                req.body.compile.drain >>
                  (if (phase == "app-after-eof") IO.raiseError[Response[IO]](boom)
                   else
                     IO.pure(
                       Response[IO](Status.ServiceUnavailable)
                         .withBodyStream(Stream.raiseError[IO](boom))
                     ))
              )
            )(declared(request.withBodyStream(source), Some(99L)))
            result <- responseF.flatMap(_.body.compile.drain).attempt
            metrics <- kit.collectMetrics
          } yield {
            assertEquals(result, Left(boom))
            val labels = start ++ protocol ++
              Option.when(phase == "response-after-eof")(
                HttpAttributes.HttpResponseStatusCode(503L)
              ) ++
              Some(ErrorAttributes.ErrorType(boom.getClass.getName))
            check(metrics, expected, labels)
          }
        }
      }
    }
  }

  List("app-before-eof", "app-after-eof", "response-after-eof").foreach { phase =>
    List(BodySizeConfig.contentLength(buckets), BodySizeConfig.observe(buckets)).foreach { settings =>
      test(s"cancellation retains only known request size: ${settings.strategy}/$phase") {
        TestControl.executeEmbed {
          OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
            for {
              entered <- Deferred[IO, Unit]
              middleware <- ServerMetrics.middleware[IO](kit, config(settings))
              stop = entered.complete(()).void >> IO.never[Unit]
              source = if (phase == "app-before-eof") payload ++ Stream.exec(stop) else payload
              app = middleware(
                HttpApp[IO](req =>
                  req.body.compile.drain >>
                    (if (phase == "response-after-eof")
                       IO.pure(Response[IO]().withBodyStream(Stream.exec(stop)))
                     else stop.as(Response[IO]()))
                )
              )
              fiber <- app(declared(request.withBodyStream(source), Some(99L)))
                .flatMap(_.body.compile.drain)
                .start
              _ <- entered.get >> fiber.cancel
              metrics <- kit.collectMetrics
            } yield check(
              metrics,
              if (settings.strategy == BodySizeConfig.Strategy.ContentLength) Some(99L)
              else Option.when(phase != "app-before-eof")(5L),
              if (phase == "response-after-eof") terminal else start ++ protocol,
            )
          }
        }
      }
    }
  }

  test(
    "a caught request-source error omits observed size without changing existing HTTP classification"
  ) {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        response <- middleware(
          HttpApp[IO](req =>
            req.body
              .handleErrorWith(_ => Stream.empty)
              .compile
              .drain
              .as(Response[IO]())
          )
        )(request.withBodyStream(payload ++ Stream.raiseError[IO](new RuntimeException("caught"))))
        _ <- response.body.compile.drain
        metrics <- kit.collectMetrics
      } yield check(metrics, None)
    }
  }

  test("request consumption deferred into the response body can reach EOF before recording") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        response <- middleware(
          HttpApp[IO](req => IO.pure(Response[IO]().withBodyStream(req.body)))
        )(request)
        bytes <- response.body.compile.toVector
        metrics <- kit.collectMetrics
      } yield {
        assertEquals(bytes, Vector[Byte](1, 2, 3, 4, 5))
        check(metrics, Some(5L))
      }
    }
  }

  test("a request still streaming at response completion cannot add a sample after late EOF") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        entered <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        source = payload ++ Stream.exec(entered.complete(()).void >> release.get)
        _ <- Resource
          .make(Ref.of[IO, Option[cats.effect.FiberIO[Unit]]](None))(
            _.get.flatMap(_.fold(IO.unit)(_.cancel))
          )
          .use { fiberRef =>
            val app = middleware(
              HttpApp[IO](req =>
                req.body.compile.drain.start.flatMap(f => fiberRef.set(Some(f))) >> entered.get
                  .as(Response[IO]())
              )
            )
            for {
              response <- app(request.withBodyStream(source))
              _ <- response.body.compile.drain
              before <- kit.collectMetrics
              _ = check(before, None)
              _ <- release.complete(()) >> fiberRef.get.flatMap(_.get.joinWithNever)
              after <- kit.collectMetrics
            } yield check(after, None)
          }
      } yield ()
    }
  }

  List(BodySizeConfig.contentLength(buckets), BodySizeConfig.observe(buckets)).foreach { settings =>
    test(s"a known request size is not published for an unstarted response: ${settings.strategy}") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config(settings))
          _ <- middleware(HttpApp[IO](req => req.body.compile.drain.as(Response[IO]())))(
            declared(request, Some(99L))
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
              .exactlyPoints(PointExpectation.numeric(1L).attributesExact(start)),
          )
        }
      }
    }
  }

  test("disabled and excluded instrumentation leaves the original request stream intact") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        effects <- Ref.of[IO, Int](0)
        req = request.withBodyStream(payload.onFinalize(effects.update(_ + 1)))
        _ <- List(
          ServerMetricsConfig.default.optIntoActiveRequests,
          config(BodySizeConfig.observe(buckets))
            .withRequestFilter(PerRequestFilter.neverEnabled),
        ).foldLeft(IO.unit) { (previous, settings) =>
          previous >> ServerMetrics.middleware[IO](kit, settings).flatMap { wrap =>
            wrap(HttpApp[IO] { received =>
              assert(received eq req)
              received.body.compile.drain.as(Response[IO]())
            })(req).flatMap(_.body.compile.drain)
          }
        }
        metrics <- kit.collectMetrics
        count <- effects.get
      } yield {
        absent(metrics)
        assertEquals(count, 2)
      }
    }
  }

  test("each invocation has independent observation state and response replay cannot record twice") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        effect = middleware(HttpApp[IO](req => req.body.compile.drain.as(Response[IO]())))(request)
        first <- effect
        second <- effect
        _ <- first.body.compile.drain >> second.body.compile.drain >> first.body.compile.drain
        metrics <- kit.collectMetrics
      } yield assertMetrics(
        metrics,
        MetricExpectation
          .histogram(spec.name)
          .unit(spec.unit)
          .exactlyPoints(
            PointExpectation.histogram
              .count(2L)
              .sum(10d)
              .boundaries(buckets)
              .attributesExact(terminal)
          ),
      )
    }
  }

  List((Method.HEAD, Status.Ok), (Method.POST, Status.NoContent)).foreach { case (method, status) =>
    test(s"response no-body rules do not replace request-body observation: $method/${status.code}") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ServerMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
          response <- middleware(
            HttpApp[IO](req => req.body.compile.drain.as(Response[IO](status)))
          )(request.withMethod(method))
          _ <- response.body.compile.drain
          metrics <- kit.collectMetrics
        } yield assertMetrics(
          metrics,
          size(
            5L,
            terminal
              .added(HttpAttributes.HttpRequestMethod(method.name))
              .added(HttpAttributes.HttpResponseStatusCode(status.code.toLong)),
            buckets,
          ),
        )
      }
    }
  }

  private def config(size: BodySizeConfig): ServerMetricsConfig =
    ServerMetricsConfig.default.optIntoActiveRequests
      .withAttributes(Attributes(Attribute("component", "server")))
      .optIntoRequestBodySize(size)

  private def declared(req: Request[IO], length: Option[Long]): Request[IO] =
    length.fold(req)(n => req.putHeaders(Header.Raw(ci"Content-Length", n.toString)))

  private def size(
      value: Long,
      labels: Attributes,
      bounds: BucketBoundaries,
  ): MetricExpectation =
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

  private def absent(metrics: List[MetricData])(implicit loc: Location): Unit =
    assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(spec.name)), None)

  private def check(
      metrics: List[MetricData],
      expected: Option[Long],
      labels: Attributes = terminal,
      bounds: BucketBoundaries = buckets,
  )(implicit loc: Location): Unit = {
    val expectations =
      List(activeMetric(0L, start), durationMetric(labels)) ++ expected.map(size(_, labels, bounds))
    assertMetrics(metrics, expectations: _*)
    if (expected.isEmpty) absent(metrics)
  }

}
