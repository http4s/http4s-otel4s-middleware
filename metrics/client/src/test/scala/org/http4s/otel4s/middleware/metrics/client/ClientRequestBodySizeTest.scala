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
import munit.CatsEffectSuite
import munit.Location
import org.http4s.Header
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.client.Client
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

class ClientRequestBodySizeTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val sizeName = HttpExperimentalMetrics.ClientRequestBodySize.name
  private val buckets = BucketBoundaries(Vector(0d, 3d, 7d))
  private val payload = Stream.emits(Vector[Byte](1, 2, 3, 4, 5)).covary[IO]
  private val request =
    Request[IO](Method.POST, uri"https://example.test/items").withBodyStream(payload)
  private val start = Attributes(
    HttpAttributes.HttpRequestMethod("POST"),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
    Attribute("component", "client"),
  )
  private val terminal = start ++ Attributes(
    HttpAttributes.HttpResponseStatusCode(200L),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

  List(
    ("declared unread", BodySizeConfig.contentLength(buckets), Some(99L), "unread", Some(99L)),
    ("declared missing", BodySizeConfig.contentLength(buckets), None, "full", None),
    ("declared zero", BodySizeConfig.contentLength(buckets), Some(0L), "unread", Some(0L)),
    ("observed mismatch", BodySizeConfig.observe(buckets), Some(99L), "full", Some(5L)),
    ("observed full", BodySizeConfig.observe(buckets), None, "full", Some(5L)),
    ("observed empty", BodySizeConfig.observe(buckets), None, "empty", Some(0L)),
    ("observed unread", BodySizeConfig.observe(buckets), Some(99L), "unread", None),
    ("observed partial", BodySizeConfig.observe(buckets), None, "partial", None),
  ).foreach { case (name, sizeConfig, length, consume, expected) =>
    test(s"body size: $name") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](kit, config(sizeConfig))
          effects <- Ref.of[IO, Int](0)
          source = (if (consume == "empty") Stream.empty else payload)
            .onFinalize(effects.update(_ + 1))
          withBody = request.withBodyStream(source)
          req = length.fold(withBody)(n =>
            withBody.putHeaders(Header.Raw(CIString("Content-Length"), n.toString))
          )
          client = middleware(Client[IO] { received =>
            if (sizeConfig.strategy == BodySizeConfig.Strategy.ContentLength)
              assert(received eq req)
            val read = consume match {
              case "unread" => IO.unit
              case "partial" => received.body.take(1).compile.drain
              case _ => received.body.compile.drain
            }
            Resource.eval(read.as(Response[IO]()))
          })
          _ <- client.run(req).use(_.body.compile.drain)
          metrics <- kit.collectMetrics
          evaluations <- effects.get
        } yield {
          check(metrics, expected, terminal)
          assertEquals(
            evaluations,
            if (consume == "unread") 0 else 1,
          )
        }
      }
    }
  }

  List(false, true).foreach { observed =>
    test(s"acquisition failure keeps a known request size: observed=$observed") {
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
          req = request.putHeaders(Header.Raw(CIString("Content-Length"), "99"))
          base = Client[IO](received =>
            Resource.eval(
              (if (observed) received.body.compile.drain else IO.unit) >> IO
                .raiseError[Response[IO]](boom)
            )
          )
          result <- middleware(base).run(req).use(_ => IO.unit).attempt
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          check(
            metrics,
            Some(if (observed) 5L else 99L),
            start.added(ErrorAttributes.ErrorType(boom.getClass.getName)),
          )
        }
      }
    }
  }

  List(false, true).foreach { cleanup =>
    test(s"request failure before observed EOF omits size: cleanup=$cleanup") {
      val boom = new RuntimeException("request failure")
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
          source =
            if (cleanup) payload.onFinalize(IO.raiseError[Unit](boom))
            else payload ++ Stream.raiseError[IO](boom)
          base = Client[IO](req => Resource.eval(req.body.compile.drain.as(Response[IO]())))
          result <- middleware(base).run(request.withBodyStream(source)).use(_ => IO.unit).attempt
          metrics <- kit.collectMetrics
        } yield {
          assertEquals(result, Left(boom))
          check(metrics, None, start.added(ErrorAttributes.ErrorType(boom.getClass.getName)))
        }
      }
    }
  }

  test("disabled and excluded requests keep the original request and create no size sample") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        disabled <- ClientMetrics.middleware[IO](kit, ClientMetricsConfig.default)
        excluded <- ClientMetrics.middleware[IO](
          kit,
          config(BodySizeConfig.observe(buckets)).withRequestFilter(PerRequestFilter.neverEnabled),
        )
        base = Client[IO] { req =>
          assert(req eq request)
          Resource.pure(Response[IO]())
        }
        _ <- disabled(base).run(request).use(_ => IO.unit)
        _ <- excluded(base).run(request).use(_ => IO.unit)
        metrics <- kit.collectMetrics
      } yield assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(sizeName)), None)
    }
  }

  List(false, true).foreach { duringRequest =>
    test(s"cancellation freezes request size: duringRequest=$duringRequest") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
        for {
          middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
          entered <- Deferred[IO, Unit]
          released <- Ref.of[IO, Int](0)
          waiting = Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
          source = (if (duringRequest) payload ++ waiting else payload)
            .onFinalize(released.update(_ + 1))
          base = Client[IO](req =>
            Resource
              .eval(req.body.compile.drain)
              .as(
                Response[IO]().withBodyStream(if (duringRequest) Stream.empty else waiting)
              )
          )
          fiber <- middleware(base)
            .run(request.withBodyStream(source))
            .use(_.body.compile.drain)
            .start
          _ <- entered.get
          _ <- fiber.cancel
          outcome <- fiber.join
          releases <- released.get
          metrics <- kit.collectMetrics
        } yield {
          assert(outcome.isCanceled)
          assertEquals(releases, 1)
          check(
            metrics,
            if (duringRequest) None else Some(5L),
            if (duringRequest) start else terminal,
          )
        }
      }
    }
  }

  test("late request EOF during backend cleanup cannot add a size sample") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      for {
        middleware <- ClientMetrics.middleware[IO](kit, config(BodySizeConfig.observe(buckets)))
        entered <- Deferred[IO, Unit]
        proceed <- Deferred[IO, Unit]
        source = payload ++ Stream.exec(entered.complete(()).void >> proceed.get)
        base = Client[IO](req =>
          Resource
            .make(req.body.compile.drain.start)(fiber =>
              proceed.complete(()).void >> fiber.joinWithNever
            )
            .evalTap(_ => entered.get)
            .as(Response[IO]())
        )
        _ <- middleware(base).run(request.withBodyStream(source)).use(_.body.compile.drain)
        metrics <- kit.collectMetrics
      } yield check(metrics, None, terminal)
    }
  }

  test("each acquisition of a reused resource has an independent observer") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { kit =>
      val sizeConfig = config(BodySizeConfig.observe(buckets))
      for {
        middleware <- ClientMetrics.middleware[IO](kit, sizeConfig)
        base = Client[IO](req => Resource.eval(req.body.compile.drain.as(Response[IO]())))
        resource = middleware(base).run(request)
        _ <- resource.use(_.body.compile.drain)
        _ <- resource.use(_.body.compile.drain)
        metrics <- kit.collectMetrics
      } yield assertMetrics(
        metrics,
        MetricExpectation
          .histogram(sizeName)
          .unit("By")
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

  private def config(size: BodySizeConfig): ClientMetricsConfig =
    ClientMetricsConfig.default.optIntoActiveRequests
      .withAttributes(Attributes(Attribute("component", "client")))
      .optIntoRequestBodySize(size)

  private def sizeExpectation(
      value: Long,
      labels: Attributes,
      bounds: BucketBoundaries,
  ): MetricExpectation =
    MetricExpectation
      .histogram(sizeName)
      .unit("By")
      .exactlyPoints(
        PointExpectation.histogram
          .count(1L)
          .sum(value.toDouble)
          .boundaries(bounds)
          .attributesExact(labels)
      )

  private def check(
      metrics: List[MetricData],
      size: Option[Long],
      labels: Attributes,
      bounds: BucketBoundaries = buckets,
  )(implicit loc: Location): Unit = {
    val base = List(activeMetric(0L, start), durationMetric(labels))
    val expectations = base ++ size.map(sizeExpectation(_, labels, bounds))
    assertMetrics(metrics, expectations: _*)
    if (size.isEmpty)
      assertEquals(MetricExpectations.find(metrics, MetricExpectation.name(sizeName)), None)
  }

}
