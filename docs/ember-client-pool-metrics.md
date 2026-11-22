# Ember client connection pool metrics

Add `http4s-otel4s-middleware-ember-client-metrics` to instrument Ember HTTP/1 pool:

```scala
libraryDependencies +=
  "org.http4s" %% "http4s-otel4s-middleware-ember-client-metrics" % "@VERSION@"
```

## Configure the provider

Create the provider with a `MeterProvider` in implicit scope:

```scala mdoc:silent
import cats.effect.Temporal
import org.http4s.otel4s.middleware.ember.client.EmberClientPoolMetrics
import org.typelevel.keypool.Metrics
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.metrics.MeterProvider

val poolConfig = EmberClientPoolMetrics.DefaultConfig
  .addConstAttributes(Attribute("pool.name", "outbound"))
  .withoutUseDuration

def poolMetrics[F[_]: Temporal: MeterProvider]: Metrics.Provider[F] =
  EmberClientPoolMetrics.provider[F](poolConfig)
```

Pass the provider to Ember's builder:

```scala
EmberClientBuilder.default[F]
  .withPoolMetricsProvider(poolMetrics[F])
  .build
```

## Instruments and attributes

All instruments below are enabled by default. Names share the prefix `http4s.ember.client.pool.`.

| Suffix | Instrument | Unit | Meaning |
| --- | --- | --- | --- |
| `connection.count` | UpDownCounter | `{connection}` | Connections by `http4s.ember.client.pool.connection.state`: `idle` or `used` |
| `acquire.pending` | UpDownCounter | `{request}` | Acquisitions waiting for a connection, including connection creation |
| `acquire.duration` | Histogram | `s` | Time until a connection is ready, or acquisition fails or is canceled |
| `connection.create.duration` | Histogram | `s` | Connection creation time, including failed or canceled attempts |
| `connection.use.duration` | Histogram | `s` | Time from checkout to return, including failed or canceled use |
| `connection.destroyed` | Counter | `{connection}` | Connections permanently removed from the pool |

Destruction counts have the attribute `http4s.ember.client.pool.destroy.reason`, with values
`idle_timeout`, `max_idle`, `max_per_key`, `not_reusable`, or `pool_closed`.
Duration histograms use the same default boundaries as HTTP request duration metrics.
Use KeyPool's configuration methods to disable individual instruments, change boundaries,
or replace instrument metadata. Constant attributes apply to every measurement; instrument
attributes take precedence. Choose a distinct, low-cardinality `pool.name` to distinguish clients.
The instrumentation scope is `org.typelevel.keypool`, with KeyPool's version.
