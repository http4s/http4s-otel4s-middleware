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
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.client.Client
import org.http4s.syntax.literals._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.OpenTelemetrySdkTestkit
import org.typelevel.otel4s.semconv.attributes.ErrorAttributes
import org.typelevel.otel4s.semconv.attributes.HttpAttributes
import org.typelevel.otel4s.semconv.attributes.NetworkAttributes
import org.typelevel.otel4s.semconv.attributes.ServerAttributes

class ClientMetricsAttributesTest extends CatsEffectSuite with ClientMetricsTestSupport {
  private val request = Request[IO](uri = uri"https://example.test/items/123")

  test(
    "labels: request labels override shared labels, are evaluated lazily once, and balance active requests"
  ) {
    OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
      var label = "captured"
      var evaluations = 0
      for {
        inFlight <- Ref.of[IO, List[MetricData]](Nil)
        middleware <- ClientMetrics.middleware[IO](
          testkit,
          ClientMetricsConfig.default.optIntoActiveRequests
            .withAttributes(
              Attributes(Attribute("component", "client"), Attribute("operation", "shared"))
            )
            .withRequestAttributes { prelude =>
              evaluations += 1
              assertEquals(prelude.uri, request.uri)
              Attributes(
                Attribute("operation", label),
                HttpAttributes.HttpRequestMethod("OVERRIDE"),
              )
            },
        )
        base = Client[IO](_ =>
          Resource.eval {
            testkit.collectMetrics.flatMap(inFlight.set) >> IO {
              label = "changed"
              Response[IO](Status.ServiceUnavailable)
            }
          }
        )
        resource = middleware(base).run(request)
        _ = assertEquals(evaluations, 0)
        _ <- resource.use(_.body.compile.drain)
        during <- inFlight.get
        metrics <- testkit.collectMetrics
      } yield {
        assertEquals(evaluations, 1)
        val labels = start() ++ Attributes(
          Attribute("component", "client"),
          Attribute("operation", "captured"),
        )
        assertMetrics(during, activeMetric(1L, labels))
        assertMetrics(
          metrics,
          activeMetric(0L, labels),
          durationMetric(
            labels ++ response(Status.ServiceUnavailable)
              .added(ErrorAttributes.ErrorType("503"))
          ),
        )
      }
    }
  }

  List("acquire-error", "body-error", "body-cancel").foreach { mode =>
    test(s"labels: request labels survive failure and cancellation: $mode") {
      OpenTelemetrySdkTestkit.inMemory[IO]().use { testkit =>
        var labelsEvaluated = 0
        var statusesEvaluated = 0
        val boom = new IllegalArgumentException("HTTP")
        val tag = Attribute("operation", "read")
        for {
          entered <- Deferred[IO, Unit]
          middleware <- ClientMetrics.middleware[IO](
            testkit,
            ClientMetricsConfig.default.optIntoActiveRequests
              .withRequestAttributes { _ => labelsEvaluated += 1; Attributes(tag) }
              .withErrorClassifier { (_, _) => statusesEvaluated += 1; false },
          )
          body =
            if (mode == "body-error") Stream.raiseError[IO](boom)
            else Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
          acquire =
            if (mode == "acquire-error") IO.raiseError[Response[IO]](boom)
            else IO.pure(Response[IO]().withBodyStream(body))
          client = middleware(Client[IO](_ => Resource.eval(acquire)))
          use = client.run(request).use(_.body.compile.drain)
          _ <-
            if (mode == "body-cancel") for {
              fiber <- use.start
              _ <- entered.get
              _ <- fiber.cancel
            } yield ()
            else use.attempt.flatMap(result => IO(assertEquals(result, Left(boom))))
          metrics <- testkit.collectMetrics
        } yield {
          assertEquals(labelsEvaluated, 1)
          assertEquals(statusesEvaluated, if (mode == "acquire-error") 0 else 1)
          val labels = start().added(tag)
          val resp = if (mode == "acquire-error") Attributes.empty else response(Status.Ok)
          val error =
            Option.when(mode != "body-cancel")(ErrorAttributes.ErrorType(boom.getClass.getName))
          assertMetrics(metrics, activeMetric(0L, labels), durationMetric(labels ++ resp ++ error))
        }
      }
    }
  }

  private def start(method: Method = Method.GET): Attributes = Attributes(
    HttpAttributes.HttpRequestMethod(method.name),
    ServerAttributes.ServerAddress("example.test"),
    ServerAttributes.ServerPort(443L),
  )

  private def response(status: Status): Attributes = Attributes(
    HttpAttributes.HttpResponseStatusCode(status.code.toLong),
    NetworkAttributes.NetworkProtocolVersion("1.1"),
  )

}
