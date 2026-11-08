# Filtering and errors

## Choose which requests to record

Metrics, tracing and propagation each have a request filter. A `PerRequestFilter` receives a
`RequestPrelude` with request metadata, including the method, URI and headers. It runs each time
the client acquires a response or the server runs the app. The response and body are not
available to the filter.

```scala mdoc:silent
import org.http4s.otel4s.middleware.{PerRequestConfig, PerRequestFilter}
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.client.ClientTracingConfig
import org.http4s.otel4s.middleware.trace.client.{ClientSpanDataProvider, UriRedactor}
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor

val excludeHealth: PerRequestFilter = request => {
  if (request.uri.path.renderString == "/health") {
    PerRequestConfig.Disabled
  } else {
    PerRequestConfig.Enabled
  }
}

val redactor = new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive {}
val spanDataProvider = ClientSpanDataProvider.openTelemetry(redactor)
val clientConfig = ClientMetricsConfig.default.withRequestFilter(excludeHealth)
val tracingConfig = ClientTracingConfig(spanDataProvider)
  .withRequestFilter(excludeHealth)
  .withPropagationFilter(PerRequestFilter.alwaysEnabled)
```

In this example, `/health` requests produce neither metrics nor spans.
`withPropagationFilter(alwaysEnabled)` allows context injection only for requests that pass the
tracing filter.

Use `and` or `or` to combine filters. Use `alwaysEnabled` or `neverEnabled` to allow or disable
every request. On the server, `withReversePropagationFilter` controls context injection into
response headers.

## Choose which responses count as errors

| Default | Client | Server |
| --- | --- | --- |
| Metrics and tracing error responses | 4xx and 5xx | 5xx |
| Cancellation alone | Neutral | Neutral |

Customize metrics and tracing separately with `withErrorClassifier`:

```scala mdoc:silent
import org.http4s.Status
import org.http4s.otel4s.middleware.ErrorClassifier

val policy = ErrorClassifier.default.excluding(Status.NotFound)
val customClientConfig = clientConfig.withErrorClassifier(policy)
val customTracingConfig = tracingConfig.withErrorClassifier(policy)
```

`ErrorClassifier.serverError` selects 5xx, and `ErrorClassifier.never` disables response-status
error classification. Use `included` to add statuses and `excluding` to remove them. Combine
classifiers with `and` or `or`.

## Exceptions and cancellation

The combined middleware and metrics-only middleware record exceptions raised while acquiring a
response, running the server app or evaluating a body. These exceptions take precedence over
errors based on the response status. `ErrorClassifier.never` does not hide them. If the
middleware sees a body failure, it records it even if code outside the middleware catches the
exception.

The middleware can only record failures it sees. For example, if a backend ends a stream
normally after a transport failure, the middleware cannot tell that the transport failed.

Errors in caller code and later client backend cleanup do not count as HTTP failures.
Cancellation alone does not set the span status to `Error` or add `error.type`. A response
status that already counts as an error still does.

See [Streaming lifecycle](streaming-lifecycle.md) for when the middleware finishes recording.
