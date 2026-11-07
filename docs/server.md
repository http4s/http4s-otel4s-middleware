# Server instrumentation

## Metrics and tracing together

Use the `server` artifact and wrap the final `HttpApp`:

```scala mdoc:silent
import cats.effect.Temporal
import cats.syntax.functor.*
import org.http4s.HttpApp
import org.http4s.otel4s.middleware.metrics.server.ServerMetricsConfig
import org.http4s.otel4s.middleware.server.{ServerInstrumentation, ServerTracingConfig}
import org.http4s.otel4s.middleware.trace.redact.{PathRedactor, QueryRedactor}
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider

val redactor = new PathRedactor.NeverRedact with QueryRedactor.RedactSensitive {}
val spanDataProvider = ServerSpanDataProvider.openTelemetry(redactor)
val serverConfig = ServerMetricsConfig.default
val tracingConfig = ServerTracingConfig(spanDataProvider)

def instrument[F[_]: Temporal: MeterProvider: TracerProvider: ContextManager](
    finalApp: HttpApp[F]
): F[HttpApp[F]] =
  ServerInstrumentation.middleware[F](serverConfig, tracingConfig).map(_(finalApp))
```

Convert routes to an app and apply response-transforming middleware before calling `instrument`.
Pass the resulting app to your server builder. This lets instrumentation observe the response
that will actually be consumed, including errors recovered by inner middleware.

## Metrics only

Use the `metrics-server` artifact:

```scala mdoc:silent
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics

def metricsOnly[F[_]: Temporal: MeterProvider: ContextManager](app: HttpApp[F]): F[HttpApp[F]] =
  ServerMetrics.middleware[F](serverConfig).map(_(app))
```

## Tracing only

Use the `trace-server` artifact:

```scala mdoc:silent
import cats.effect.MonadCancelThrow
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware

def tracingOnly[F[_]: MonadCancelThrow: TracerProvider](app: HttpApp[F]): F[HttpApp[F]] =
  ServerMiddleware.builder[F](spanDataProvider).build.map(_.wrapHttpApp(app))
```

The tracing-only builder remains a separate API. The lifecycle rules below describe combined
instrumentation and standalone metrics.

## Route classification

Supply a low-cardinality route template to both metrics and tracing. Classification is explicit;
the library does not infer the route matched by your application:

```scala mdoc:silent
import cats.effect.IO
import org.http4s.dsl.io.*
import org.http4s.otel4s.middleware.server.RouteClassifier

val routes = RouteClassifier.of[IO] {
  case GET -> Root / "users" / _ => "/users/{id}"
}
val classifiedMetricsConfig = serverConfig.withRouteClassifier(routes)
val classifiedSpanDataProvider = spanDataProvider.withRouteClassifier(routes)
val classifiedTracingConfig = tracingConfig.withSpanDataProvider(classifiedSpanDataProvider)
```

An unclassified route is omitted; do not substitute the raw request path.

## Completion and propagation

App failure or cancellation completes instrumentation immediately. A successful app response
completes only when its body is evaluated and terminates, including cleanup. This also applies to
empty and HEAD responses. An outer middleware that discards the instrumented body can leave the
span open and active-request count incremented. Body completion does not establish delivery to
the remote client.

App and response body effects run in the context extracted from request headers, including
baggage. Response context propagation is disabled by default; enable it with
`tracingConfig.withReversePropagationFilter(...)` if your protocol requires it.

See [Lifecycle and limitations](lifecycle.md) for scenarios and
[Tracing and propagation](tracing.md) for privacy and context configuration.
