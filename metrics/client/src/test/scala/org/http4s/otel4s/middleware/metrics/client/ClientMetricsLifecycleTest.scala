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
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
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
import org.typelevel.otel4s.semconv.metrics.HttpMetrics

import scala.concurrent.duration._

class ClientMetricsLifecycleTest extends CatsEffectSuite with ClientMetricsTestSupport {
  import ClientMetricsLifecycleTest.Scenario

  private val request = Request[IO](uri = uri"https://example.test/items")
  private val custom = Attribute("test.tag", "present")
  private val startAttributes = Attributes(
    custom,
    HttpAttributes.HttpRequestMethod(HttpAttributes.HttpRequestMethodValue.Get),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  List(
    Scenario("success", expectedDuration = 10.0),
    Scenario("partial", expectedDuration = 10.0),
    Scenario("unread", expectedDuration = 9.0),
    Scenario("consumer-error", expectedDuration = 9.0, fails = true),
    Scenario("post-body-error", expectedDuration = 10.0, fails = true),
    Scenario(
      "body-error",
      expectedDuration = 10.0,
      fails = true,
      errorType = Some(classOf[RuntimeException].getName),
    ),
    Scenario(
      "caught-body-error",
      expectedDuration = 10.0,
      errorType = Some(classOf[RuntimeException].getName),
    ),
    Scenario(
      "acquire-error",
      expectedDuration = 2.0,
      fails = true,
      hasResponse = false,
      errorType = Some(classOf[RuntimeException].getName),
    ),
    Scenario("acquire-cancel", expectedDuration = 2.0, hasResponse = false, canceled = true),
    Scenario("body-cancel", expectedDuration = 10.0, canceled = true),
    Scenario("replay", expectedDuration = 10.0),
    Scenario("error-status", expectedDuration = 10.0, errorType = Some("503")),
    Scenario(
      "cleanup-error",
      expectedDuration = 10.0,
      fails = true,
      errorType = Some(classOf[RuntimeException].getName),
    ),
    Scenario("partial-cleanup-error", expectedDuration = 10.0, fails = true),
    Scenario("downstream-error", expectedDuration = 10.0, fails = true),
    Scenario("release-error", expectedDuration = 10.0, fails = true),
  ).foreach { scenario =>
    val mode = scenario.name
    test(s"lifecycle: records one duration and balances active requests: $mode") {
      TestControl.executeEmbed {
        OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
          for {
            middleware <- ClientMetrics.middleware[IO](
              testkit,
              ClientMetricsConfig.default.optIntoActiveRequests.withAttributes(Attributes(custom)),
            )
            entered <- Deferred[IO, Unit]
            during <- Ref.of[IO, List[MetricData]](Nil)
            beforeRelease <- Ref.of[IO, List[MetricData]](Nil)
            boom = new RuntimeException("HTTP or consumer failure")
            status = if (mode == "error-status") Status.ServiceUnavailable else Status.Ok
            cleanup = IO.sleep(
              5.seconds
            ) >> (if (Set("cleanup-error", "partial-cleanup-error")(mode))
                    IO.raiseError[Unit](boom)
                  else IO.unit)
            body = (Stream.sleep_[IO](3.seconds) ++
              (if (mode == "body-cancel")
                 Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
               else
                 Stream.emits(List[Byte](1, 2, 3)).covary[IO] ++
                   (if (Set("body-error", "caught-body-error")(mode)) Stream.raiseError[IO](boom)
                    else Stream.empty)))
              .onFinalize(cleanup)
            acquire = IO.sleep(2.seconds) >> testkit.collectMetrics.flatMap(during.set) >>
              (mode match {
                case "acquire-error" => IO.raiseError[Unit](boom)
                case "acquire-cancel" => entered.complete(()).void >> IO.never[Unit]
                case _ => IO.unit
              })
            base = Client[IO](_ =>
              Resource.makeFull[IO, Response[IO]](poll =>
                poll(acquire).as(Response[IO](status).withBodyStream(body))
              )(_ =>
                testkit.collectMetrics.flatMap(beforeRelease.set) >> IO.sleep(11.seconds) >>
                  (if (mode == "release-error") IO.raiseError[Unit](boom) else IO.unit)
              )
            )
            use = middleware(base).run(request).use { response =>
              mode match {
                case "unread" => IO.sleep(7.seconds)
                case "consumer-error" => IO.sleep(7.seconds) >> IO.raiseError[Unit](boom)
                case "post-body-error" =>
                  response.body.compile.drain >> IO.sleep(7.seconds) >> IO.raiseError[Unit](boom)
                case "downstream-error" =>
                  response.body.evalMap(_ => IO.raiseError[Byte](boom)).compile.drain
                case "partial-cleanup-error" => response.body.take(1).compile.drain
                case "partial" =>
                  response.body
                    .take(1)
                    .compile
                    .toVector
                    .flatMap(b => IO(assertEquals(b, Vector[Byte](1))))
                case "caught-body-error" =>
                  response.body.handleErrorWith(_ => Stream.empty).compile.drain
                case "replay" => response.body.compile.drain >> response.body.compile.drain
                case _ => response.body.compile.drain
              }
            }
            _ <-
              if (scenario.canceled) for {
                fiber <- use.start
                _ <- entered.get
                _ <- fiber.cancel
              } yield ()
              else
                use.attempt.flatMap { result =>
                  IO(assertEquals(result, if (scenario.fails) Left(boom) else Right(())))
                }
            inFlight <- during.get
            atRelease <- beforeRelease.get
            metrics <- testkit.collectMetrics
          } yield {
            val baseAttrs = if (scenario.hasResponse) terminal(status) else startAttributes
            val error = scenario.errorType.map(ErrorAttributes.ErrorType(_))
            val expected = MetricExpectation
              .histogram(HttpMetrics.ClientRequestDuration.name)
              .unit("s")
              .exactlyPoints(
                PointExpectation.histogram
                  .count(1L)
                  .sum(scenario.expectedDuration)
                  .exemplarCount(0)
                  .attributesExact(baseAttrs ++ error)
              )
            assertMetrics(inFlight, active(1L))
            assertMetrics(metrics, active(0L), expected)
            if (scenario.hasResponse) assertMetrics(atRelease, active(0L), expected)
          }
        }
      }
    }
  }

  test("body: an observed body exception takes precedence over an error response status") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      val boom = new IllegalStateException("body")
      for {
        middleware <- ClientMetrics
          .middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests.withAttributes(Attributes(custom)),
          )
        base = Client[IO](_ =>
          Resource.pure(
            Response[IO](Status.ServiceUnavailable)
              .withBodyStream(Stream.raiseError[IO](boom))
          )
        )
        result <- middleware(base).run(request).use(_.body.compile.drain).attempt
        metrics <- testkit.collectMetrics
      } yield {
        assertEquals(result, Left(boom))
        assertMetrics(
          metrics,
          active(0L),
          MetricExpectation
            .histogram(HttpMetrics.ClientRequestDuration.name)
            .exactlyPoints(
              PointExpectation.histogram
                .count(1L)
                .attributesExact(
                  terminal(Status.ServiceUnavailable)
                    .added(ErrorAttributes.ErrorType(boom.getClass.getName))
                )
            ),
        )
      }
    }
  }

  test("acquisition: resource construction is lazy and each acquisition creates a new exchange") {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      for {
        middleware <- ClientMetrics
          .middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests.withAttributes(Attributes(custom)),
          )
        base = Client[IO](_ => Resource.pure(Response[IO]()))
        response = middleware(base).run(request)
        before <- testkit.collectMetrics
        _ = assertEquals(
          MetricExpectations
            .find(before, MetricExpectation.name(HttpMetrics.ClientRequestDuration.name)),
          None,
        )
        _ = assertEquals(
          MetricExpectations.find(
            before,
            MetricExpectation.name(HttpExperimentalMetrics.ClientActiveRequests.name),
          ),
          None,
        )
        _ <- response.use(_.body.compile.drain)
        _ <- response.use(_.body.compile.drain)
        metrics <- testkit.collectMetrics
      } yield assertMetrics(
        metrics,
        active(0L),
        MetricExpectation
          .histogram(HttpMetrics.ClientRequestDuration.name)
          .exactlyPoints(PointExpectation.histogram.count(2L).attributesExact(terminal(Status.Ok))),
      )
    }
  }

  private def terminal(status: Status): Attributes = startAttributes ++ Attributes(
    HttpAttributes.HttpResponseStatusCode(status.code.toLong),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

  private def active(value: Long): MetricExpectation =
    MetricExpectation
      .sum[Long](HttpExperimentalMetrics.ClientActiveRequests.name)
      .exactlyPoints(PointExpectation.numeric(value).attributesExact(startAttributes))

}

private object ClientMetricsLifecycleTest {
  final case class Scenario(
      name: String,
      expectedDuration: Double,
      fails: Boolean = false,
      hasResponse: Boolean = true,
      canceled: Boolean = false,
      errorType: Option[String] = None,
  )
}
