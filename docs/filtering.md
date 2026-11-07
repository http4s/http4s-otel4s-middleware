# Filtering and errors

## Select requests independently

Metrics, tracing and propagation have separate policies. `PerRequestFilter` receives the
`RequestPrelude`, so selection is based on request metadata rather than the response or body.
It is evaluated for each client acquisition or server app execution.

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

The last setting permits propagation for traced requests; it does not override a disabled tracing
filter. Server tracing has `withReversePropagationFilter` for response headers. Compose filters
with `and` or `or`, and use `alwaysEnabled` or `neverEnabled` for fixed policies.

## Classify response statuses

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
error classification. `included` adds statuses, `excluding` removes them; classifiers also
compose with `and` and `or`.

## Exceptions and cancellation

For combined instrumentation and standalone metrics, observed acquisition/app and body exceptions
take precedence over a response-status classification. Setting `ErrorClassifier.never` does not
hide those exceptions. An observed body failure remains an HTTP failure even if downstream code
catches it. A backend can expose a transport failure as normal stream termination; instrumentation
can classify only the outcome visible at its boundary.

Caller failures outside the client body and errors in the client's later backend release are not
classified as HTTP failures. Cancellation alone does not set error status or `error.type`; a
response status already classified as an error can still make the exchange an error.

See [Lifecycle and limitations](lifecycle.md) for exactly which work each boundary includes.
