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
  ServerInstrumentation
    .middleware[F](serverConfig, tracingConfig)
    .map(middleware => middleware(finalApp))
```

Convert routes to an app, then apply error handlers and any middleware that changes the
response. Pass that app to `instrument`, then give the wrapped app to your server builder. The
instrumentation will see the final response, including responses returned by error handlers.

## Metrics only

Use the `metrics-server` artifact:

```scala mdoc:silent
import org.http4s.otel4s.middleware.metrics.server.ServerMetrics

def metricsOnly[F[_]: Temporal: MeterProvider: ContextManager](app: HttpApp[F]): F[HttpApp[F]] =
  ServerMetrics
    .middleware[F](serverConfig)
    .map(middleware => middleware(app))
```

## Tracing only

Use the `trace-server` artifact:

```scala mdoc:silent
import cats.effect.MonadCancelThrow
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware

def tracingOnly[F[_]: MonadCancelThrow: TracerProvider](app: HttpApp[F]): F[HttpApp[F]] =
  ServerMiddleware
    .builder[F](spanDataProvider)
    .build
    .map(middleware => middleware.wrapHttpApp(app))
```

The tracing-only builder ends its span when the app returns a response. The combined middleware
keeps it open until the response body finishes, including cleanup.

## Route templates

Give metrics and tracing the same route template, such as `/users/{id}`. The library does not
know which route your app matched, so you need to provide a `RouteClassifier`:

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

If the classifier does not match, the route attribute is omitted. Avoid using raw paths with IDs
or other values that change for each request.

## When recording finishes

The combined middleware and metrics-only middleware finish recording immediately if the app
fails or is canceled. If the app returns a response, recording finishes only after its body runs
and finishes, fails or is canceled, including cleanup. Empty and HEAD response bodies must run
too.

If an outer middleware replaces the response without running its original body, the span can
stay open and the active-request count can stay at one. Finishing the body does not prove that
the remote client received the bytes.

## Request and response context

With tracing enabled, app and response body effects run in the context read from request
headers, including baggage. Adding context to response headers is disabled by default. To enable
it, use `tracingConfig.withReversePropagationFilter(...)`.

See [Streaming lifecycle](streaming-lifecycle.md) for completion rules and [Tracing and
propagation](tracing.md) for privacy and context configuration.
