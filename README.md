# http4s-otel4s-middleware

## Quick Start

To use http4s-otel4s-middleware in an existing SBT project with Scala 2.13 or a
later version, add one or more of the following dependencies to your `build.sbt`
depending on your needs:

```scala
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-otel4s-middleware-core" % "<version>",
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
metric with `ClientMetricsConfig.default.optIntoActiveRequests`:

```scala
import cats.effect.Temporal
import cats.syntax.functor._
import org.http4s.client.Client
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.typelevel.otel4s.Otel4s

def instrument[F[_]: Temporal](otel4s: Otel4s[F], client: Client[F]): F[Client[F]] =
  ClientMetrics.middleware[F](otel4s, ClientMetricsConfig.default).map(wrap => wrap(client))
```

| Instrument | Completion / labels |
| --- | --- |
| `http.client.request.duration` | Body termination, or response release when unread, in seconds. |
| `http.client.active_requests` | Opt-in with `optIntoActiveRequests`; increment before acquisition and decrement once at completion. |
| `http.client.request.body.size` | Opt-in with `optIntoRequestBodySize`; request body size in bytes. |
| `http.client.response.body.size` | Opt-in with `optIntoResponseBodySize`; response body size in bytes. |

## Server metrics middleware

Use the `metrics-server` artifact for server duration metrics:

```scala
import cats.effect.Temporal
import cats.syntax.functor._
import org.http4s.HttpApp
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.typelevel.otel4s.Otel4s

def instrumentServer[F[_]: Temporal](otel4s: Otel4s[F], app: HttpApp[F]): F[HttpApp[F]] =
  ServerMetrics.middleware[F](otel4s, ServerMetricsConfig.default).map(wrap => wrap(app))
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
