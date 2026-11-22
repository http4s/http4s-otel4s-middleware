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

import cats.effect.kernel.Temporal
import org.typelevel.keypool.Metrics
import org.typelevel.keypool.otel4s.Otel4sMetrics
import org.typelevel.keypool.otel4s.Otel4sMetrics.InstrumentConfig
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeterProvider

import java.util.concurrent.TimeUnit

/** OpenTelemetry metrics for Ember's HTTP/1 connection pool.
  *
  * Pass [[provider]] to `EmberClientBuilder.withPoolMetricsProvider`.
  * Measurements aggregate across all destinations in the pool. HTTP/2 connections are excluded.
  */
object EmberClientPoolMetrics {

  /** All pool instruments enabled, using seconds for durations.
    *
    * Customize with KeyPool's configuration methods to add constant attributes, disable
    * instruments, or replace instrument names and histogram boundaries. The instrumentation
    * scope and version belong to KeyPool, which implements the measurements.
    */
  val DefaultConfig: Otel4sMetrics.Config = {
    val defaults = Otel4sMetrics.Config.Defaults
    val prefix = "http4s.ember.client.pool"
    val stateKey = "http4s.ember.client.pool.connection.state"

    def duration(name: String, description: String): InstrumentConfig.Histogram =
      InstrumentConfig.histogram(
        name = s"$prefix.$name",
        timeUnit = TimeUnit.SECONDS,
        description = description,
        attributes = Attributes.empty,
        explicitBucketBoundaries = defaults.histogramBucketBoundaries,
      )

    Otel4sMetrics.Config.default
      .withIdleInstrument(
        InstrumentConfig.upDownCounter(
          name = s"$prefix.connection.count",
          unit = "{connection}",
          description = "Number of HTTP/1 connections in the pool, by state.",
          attributes = Attributes(Attribute(stateKey, "idle")),
        )
      )
      .withInUseInstrument(
        InstrumentConfig.upDownCounter(
          name = s"$prefix.connection.count",
          unit = "{connection}",
          description = "Number of HTTP/1 connections in the pool, by state.",
          attributes = Attributes(Attribute(stateKey, "used")),
        )
      )
      .withPendingAcquireInstrument(
        InstrumentConfig.upDownCounter(
          name = s"$prefix.acquire.pending",
          unit = "{request}",
          description = "Number of requests waiting to acquire an HTTP/1 connection.",
          attributes = Attributes.empty,
        )
      )
      .withAcquireDurationInstrument(
        duration(
          "acquire.duration",
          "Time taken to obtain an HTTP/1 connection from the pool.",
        )
      )
      .withCreateDurationInstrument(
        duration(
          "connection.create.duration",
          "Time taken to create an HTTP/1 connection.",
        )
      )
      .withUseDurationInstrument(
        duration(
          "connection.use.duration",
          "Duration between borrowing an HTTP/1 connection and returning it to the pool.",
        )
      )
      .withDestroyedInstrument(
        InstrumentConfig.counter(
          name = s"$prefix.connection.destroyed",
          unit = "{connection}",
          description = "Number of HTTP/1 connections removed permanently from the pool.",
          attributes = reason =>
            Attributes(
              Attribute(
                "http4s.ember.client.pool.destroy.reason",
                reason match {
                  case Metrics.DestructionReason.IdleTimeout => "idle_timeout"
                  case Metrics.DestructionReason.MaxIdle => "max_idle"
                  case Metrics.DestructionReason.MaxPerKey => "max_per_key"
                  case Metrics.DestructionReason.NotReusable => "not_reusable"
                  case Metrics.DestructionReason.PoolClosed => "pool_closed"
                },
              )
            ),
        )
      )
  }

  /** Creates a provider with the default pool instruments. */
  def provider[F[_]: Temporal: MeterProvider]: Metrics.Provider[F] =
    provider[F](DefaultConfig)

  /** Creates instruments each time Ember builds a client. */
  def provider[F[_]: Temporal: MeterProvider](
      config: Otel4sMetrics.Config
  ): Metrics.Provider[F] =
    Otel4sMetrics.provider[F](config)
}
