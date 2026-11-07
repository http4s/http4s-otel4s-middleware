# http4s-otel4s-middleware

OpenTelemetry metrics and tracing for http4s clients and servers, built on
[otel4s](https://typelevel.org/otel4s/). Supports Scala 2.13 and Scala 3 on JVM, Scala.js and Scala Native.

[Documentation](https://http4s.github.io/http4s-otel4s-middleware/).

## Quick Start

To use http4s-otel4s-middleware in an existing SBT project with Scala 2.13 or a
later version, add one or more of the following dependencies to your `build.sbt`
depending on your needs:

```scala
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-otel4s-middleware-core" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-client" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-server" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-metrics-client" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-metrics-server" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-metrics" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-trace-core" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-trace-client" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-trace-server" % "<version>",
)
```

## Client metrics middleware

Use the `metrics-client` artifact for duration metrics. Enable the experimental active-request
metric with `ClientMetricsConfig.default.optIntoActiveRequests`. Supply the meter provider and context
manager from the same telemetry instance in implicit scope:

```scala
import cats.effect.Temporal
import cats.syntax.functor._
import org.http4s.client.Client
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider

def instrument[F[_]: Temporal: MeterProvider: ContextManager](client: Client[F]): F[Client[F]] =
  ClientMetrics
    .middleware[F](ClientMetricsConfig.default)
    .map(wrap => wrap(client))
```

| Instrument | Completion / labels |
| --- | --- |
| `http.client.request.duration` | Body termination, or response release when unread, in seconds. |
| `http.client.active_requests` | Opt-in with `optIntoActiveRequests`; increment before acquisition and decrement once at completion. |
| `http.client.request.body.size` | Opt-in with `optIntoRequestBodySize`; request body size in bytes. |
| `http.client.response.body.size` | Opt-in with `optIntoResponseBodySize`; response body size in bytes. |

## Client metrics and tracing

Use the `client` artifact to enable metrics and tracing together. It records metrics before ending
each HTTP span. Pass a config for each:

```scala
import cats.effect.Temporal
import cats.syntax.functor._
import org.http4s.client.Client
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import org.http4s.otel4s.middleware.client.ClientInstrumentation
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.trace.client.ClientSpanDataProvider
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor

val tracingConfig = ClientTracingConfig(
  ClientSpanDataProvider.openTelemetry(
    new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive {}
  )
)

def instrumentClient[F[_]: Temporal: MeterProvider: TracerProvider: ContextManager](
    client: Client[F]
): F[Client[F]] =
  ClientInstrumentation
    .middleware[F](ClientMetricsConfig.default, tracingConfig)
    .map(wrap => wrap(client))
```

## Server metrics middleware

Use the `metrics-server` artifact for server duration metrics:

```scala
import cats.effect.Temporal
import cats.syntax.functor._
import org.http4s.HttpApp
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider

def instrumentServer[F[_]: Temporal: MeterProvider: ContextManager](app: HttpApp[F]): F[HttpApp[F]] =
  ServerMetrics
    .middleware[F](ServerMetricsConfig.default)
    .map(wrap => wrap(app))
```

| Instrument | Boundary |
| --- | --- |
| `http.server.request.duration` | App failure/cancellation, or response-body termination including cleanup, in seconds. |
| `http.server.active_requests` | Opt-in with `optIntoActiveRequests`. Increment before the app and decrement once at completion. |
| `http.server.request.body.size` | Opt-in with `optIntoRequestBodySize`; request body size in bytes. |
| `http.server.response.body.size` | Opt-in with `optIntoResponseBodySize`; response body size in bytes. |

**Unstarted-body limitation:** returning a response without evaluating its body leaves duration
and body sizes unrecorded and active requests incremented. This includes empty and HEAD responses.
App errors and app cancellation still complete immediately. Body completion does not establish network delivery.

## Server metrics and tracing

Use the `server` artifact to enable metrics and tracing together. It records metrics before ending
each HTTP span. Pass a config for each:

```scala
import cats.effect.Temporal
import cats.syntax.functor._
import org.http4s.HttpApp
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.ServerInstrumentation
import org.http4s.otel4s.middleware.server.ServerTracingConfig
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider

val serverTracingConfig = ServerTracingConfig(
  ServerSpanDataProvider.openTelemetry(
    new PathRedactor.NeverRedact with QueryRedactor.RedactSensitive {}
  )
)

def instrumentServerApp[F[_]: Temporal: MeterProvider: TracerProvider: ContextManager](
    app: HttpApp[F]
): F[HttpApp[F]] =
  ServerInstrumentation
    .middleware[F](ServerMetricsConfig.default, serverTracingConfig)
    .map(wrap => wrap(app))
```

Apply once to the final app. App and response body effects run in the context extracted from the
request headers. Code consuming the body keeps its own context. Metrics and tracing have separate
request filters and error classifiers. Response context propagation is disabled by default; enable
it with `withReversePropagationFilter`.

The response body must be run, including for empty and HEAD responses. Until then, the span stays
open, the request counts as active, and duration and body sizes are not recorded.
