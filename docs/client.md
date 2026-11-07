# Client instrumentation

## Metrics and tracing together

Use the `client` artifact. Construct the middleware once and reuse the wrapped client:

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
    raw: Client[F]
): F[Client[F]] =
  ClientInstrumentation.middleware[F](clientConfig, tracingConfig).map(_(raw))
```

Supply providers from the same backend, as in [Getting started](getting-started.md).
The combined middleware records metrics before ending its HTTP span, enabling exemplar
correlation when supported and configured by the backend.

## Metrics only

Use the `metrics-client` artifact. It needs a meter provider and context manager:

```scala mdoc:silent
import org.http4s.otel4s.middleware.metrics.client.ClientMetrics

def metricsOnly[F[_]: Temporal: MeterProvider: ContextManager](raw: Client[F]): F[Client[F]] =
  ClientMetrics.middleware[F](clientConfig).map(_(raw))
```

## Tracing only

Use the `trace-client` artifact and its builder:

```scala mdoc:silent
import cats.effect.MonadCancelThrow
import org.http4s.otel4s.middleware.trace.client.ClientMiddleware

def tracingOnly[F[_]: MonadCancelThrow: TracerProvider](raw: Client[F]): F[Client[F]] =
  ClientMiddleware.builder[F](spanDataProvider).build.map(_.wrapClient(raw))
```

The tracing-only builder is a separate API; its completion behavior should not be assumed to
match the new combined middleware. See [Migrating to 0.20](migration.md).

## Streaming and resource ownership

For combined instrumentation and standalone metrics, completion occurs when the response body
terminates, or when the response resource is released if the body is unread. Release the resource
even when you do not need the body. The middleware does not drain unread bodies for you.

Request and response body effects run in the HTTP span's context. Code consuming the response
body retains its own context. HTTP failures observed in acquisition or body processing are distinct
from errors in caller code or backend resource release.

Instrument the client once. Putting retry logic outside the instrumented client produces a
separate exchange for each acquisition attempt. Use low-cardinality URL templates rather than raw
paths for metric labels. See [Metrics](metrics.md), [Tracing](tracing.md) and
[Lifecycle and limitations](lifecycle.md).
