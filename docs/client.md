# Client instrumentation

## Metrics and tracing together

Use the `client` artifact. Create the middleware once and reuse the wrapped client:

```scala mdoc:silent
import cats.effect.Temporal
import cats.syntax.functor.*
import org.http4s.client.Client
import org.http4s.otel4s.middleware.client.{ClientInstrumentation, ClientTracingConfig}
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.{ClientSpanDataProvider, UriRedactor}
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider

val redactor = new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive {}
val spanDataProvider = ClientSpanDataProvider.openTelemetry(redactor)
val clientConfig = ClientMetricsConfig.default
val tracingConfig = ClientTracingConfig(spanDataProvider)

def instrument[F[_]: Temporal: MeterProvider: TracerProvider: ContextManager](
    client: Client[F]
): F[Client[F]] =
  ClientInstrumentation
    .middleware[F](clientConfig, tracingConfig)
    .map(middleware => middleware(client))
```

Use providers from the same telemetry instance, as in [Getting started](getting-started.md). The
middleware records metrics before ending the HTTP span. If your backend supports exemplars, it
can link those measurements to the span.

## Metrics only

Use the `metrics-client` artifact. It needs a meter provider and context manager:

```scala mdoc:silent
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics

def metricsOnly[F[_]: Temporal: MeterProvider: ContextManager](client: Client[F]): F[Client[F]] =
  ClientMetrics
    .middleware[F](clientConfig)
    .map(middleware => middleware(client))
```

## Tracing only

Use the `trace-client` artifact and its builder:

```scala mdoc:silent
import cats.effect.MonadCancelThrow
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware

def tracingOnly[F[_]: MonadCancelThrow: TracerProvider](client: Client[F]): F[Client[F]] =
  ClientMiddleware
    .builder[F](spanDataProvider)
    .build
    .map(middleware => middleware.wrapClient(client))
```

The tracing-only builder ends its span when the response resource is released. The combined
middleware can end it earlier, when the body finishes. See [Migrating to 0.20](migration.md).

## Reading and releasing responses

The combined middleware and metrics-only middleware finish recording when the response body
finishes, fails or is canceled, including its cleanup. If the body is never read, they finish
recording when the response resource is released. Always release the response resource, even
when you do not need the body. The middleware does not read unread bodies for you.

With tracing enabled, request and response body effects run in the HTTP span's context. The code
that consumes the response body keeps its own context.

Errors while acquiring the response or running its body count as HTTP failures. Errors in caller
code or later backend resource cleanup do not.

Instrument the client once. If retry logic wraps the instrumented client, each attempt gets its
own span and measurements. Use URL templates such as `/users/{id}` for metric labels. Raw paths
containing IDs create a separate metric series for every value. See [Metrics](metrics.md),
[Tracing](tracing.md) and [Streaming lifecycle](streaming-lifecycle.md).
