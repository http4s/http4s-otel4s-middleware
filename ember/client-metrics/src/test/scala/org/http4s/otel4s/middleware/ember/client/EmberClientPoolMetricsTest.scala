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

package org.http4s.otel4s.middleware.ember.client

import cats.effect.IO
import cats.effect.Resource
import cats.effect.testkit.TestControl
import cats.syntax.all._
import munit.CatsEffectSuite
import org.typelevel.keypool.KeyPool
import org.typelevel.keypool.Metrics
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectation
import org.typelevel.otel4s.sdk.testkit.metrics.MetricExpectations
import org.typelevel.otel4s.sdk.testkit.metrics.MetricsTestkit
import org.typelevel.otel4s.sdk.testkit.metrics.PointExpectation

import scala.concurrent.duration._

class EmberClientPoolMetricsTest extends CatsEffectSuite {
  private val prefix = "http4s.ember.client.pool"
  private val poolName = Attribute("pool.name", "test")
  private val config =
    EmberClientPoolMetrics.DefaultConfig.withConstAttributes(Attributes(poolName))

  test("pool reuse, acquisition, durations and shutdown use Ember instruments and attributes") {
    TestControl.executeEmbed {
      MetricsTestkit.inMemory[IO]().use { sdk =>
        implicit val meterProvider: MeterProvider[IO] = sdk.meterProvider
        for {
          created <- IO.ref(0)
          _ <- KeyPool
            .Builder[IO, String, Unit](_ =>
              Resource.eval(created.update(_ + 1) >> IO.sleep(10.millis))
            )
            .withMetricsProvider(EmberClientPoolMetrics.provider[IO](config))
            .build
            .use { pool =>
              for {
                _ <- pool.take("host").use { _ =>
                  sdk.collectMetrics.flatMap { metrics =>
                    IO(assertMetrics(metrics, count("used", 1), pending(0))) >> IO.sleep(20.millis)
                  }
                }
                _ <- pool.take("host").use_
                metrics <- sdk.collectMetrics
              } yield assertMetrics(metrics, count("idle", 1), count("used", 0))
            }
          creations <- created.get
          metrics <- sdk.collectMetrics
        } yield {
          assertEquals(creations, 1)
          assertEquals(
            metrics.map(_.name).toSet,
            Set(
              s"$prefix.connection.count",
              s"$prefix.acquire.pending",
              s"$prefix.acquire.duration",
              s"$prefix.connection.create.duration",
              s"$prefix.connection.use.duration",
              s"$prefix.connection.destroyed",
            ),
          )
          assertMetrics(
            metrics,
            count("idle", 0),
            count("used", 0),
            pending(0),
            histogram("acquire.duration", 2),
            histogram("connection.create.duration", 1),
            histogram("connection.use.duration", 2),
            MetricExpectation
              .sum[Long](s"$prefix.connection.destroyed")
              .exactlyPoints(
                PointExpectation
                  .numeric(1L)
                  .attributesExact(
                    poolName,
                    Attribute("http4s.ember.client.pool.destroy.reason", "pool_closed"),
                  )
              ),
          )
        }
      }
    }
  }

  test("failed and canceled acquisitions balance pending requests and record once") {
    TestControl.executeEmbed {
      MetricsTestkit.inMemory[IO]().use { sdk =>
        implicit val meterProvider: MeterProvider[IO] = sdk.meterProvider
        for {
          started <- IO.deferred[Unit]
          _ <- KeyPool
            .Builder[IO, String, Unit] {
              case "fail" => Resource.eval(IO.raiseError[Unit](new RuntimeException("connect")))
              case _ => Resource.eval(started.complete(()).void >> IO.never[Unit])
            }
            .withMetricsProvider(EmberClientPoolMetrics.provider[IO](config))
            .build
            .use { pool =>
              for {
                failed <- pool.take("fail").use_.attempt
                _ = assert(failed.isLeft)
                fiber <- pool.take("cancel").use_.start
                _ <- started.get
                waiting <- sdk.collectMetrics
                _ = assertMetrics(waiting, pending(1))
                _ <- fiber.cancel
              } yield ()
            }
          metrics <- sdk.collectMetrics
        } yield assertMetrics(metrics, pending(0), histogram("acquire.duration", 2))
      }
    }
  }

  test("configuration can disable instruments and preserve destruction reasons") {
    MetricsTestkit.inMemory[IO]().use { sdk =>
      implicit val meterProvider: MeterProvider[IO] = sdk.meterProvider
      val destroyedOnly =
        config.withoutIdle.withoutInUse.withoutPendingAcquire.withoutAcquireDuration.withoutCreateDuration.withoutUseDuration
      for {
        metrics <- EmberClientPoolMetrics.provider[IO](destroyedOnly).get
        _ <- List(
          Metrics.DestructionReason.IdleTimeout,
          Metrics.DestructionReason.MaxIdle,
          Metrics.DestructionReason.MaxPerKey,
          Metrics.DestructionReason.NotReusable,
          Metrics.DestructionReason.PoolClosed,
        ).traverse_(metrics.resourceDestroyed)
        _ <- metrics.acquire.use(_.complete)
        _ <- metrics.inUseCount.use_
        _ <- metrics.idleInc >> metrics.idleDec
        _ <- metrics.createDuration.use_ >> metrics.useDuration.use_
        collected <- sdk.collectMetrics
      } yield {
        assertEquals(collected.map(_.name), List(s"$prefix.connection.destroyed"))
        val points = List("idle_timeout", "max_idle", "max_per_key", "not_reusable", "pool_closed")
          .map(reason =>
            PointExpectation
              .numeric(1L)
              .attributesExact(
                poolName,
                Attribute("http4s.ember.client.pool.destroy.reason", reason),
              )
          )
        assertMetrics(
          collected,
          MetricExpectation
            .sum[Long](s"$prefix.connection.destroyed")
            .exactlyPoints(points.head, points.tail: _*),
        )
      }
    }
  }

  private def count(state: String, value: Long): MetricExpectation =
    MetricExpectation
      .sum[Long](s"$prefix.connection.count")
      .containsPoints(
        PointExpectation
          .numeric(value)
          .attributesExact(
            poolName,
            Attribute("http4s.ember.client.pool.connection.state", state),
          )
      )

  private def pending(value: Long): MetricExpectation =
    MetricExpectation
      .sum[Long](s"$prefix.acquire.pending")
      .exactlyPoints(
        PointExpectation.numeric(value).attributesExact(poolName)
      )

  private def histogram(name: String, count: Long): MetricExpectation =
    MetricExpectation
      .histogram(s"$prefix.$name")
      .exactlyPoints(
        PointExpectation.histogram.count(count).attributesExact(poolName)
      )

  private def assertMetrics(metrics: List[MetricData], expectations: MetricExpectation*): Unit =
    MetricExpectations
      .checkAll(metrics, expectations: _*)
      .fold(mismatches => fail(MetricExpectations.format(mismatches)), identity)
}
