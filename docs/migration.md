# Migrating to 0.20

Version 0.20 adds modules for metrics alone and for metrics and tracing together. This guide
explains how they differ from the existing tracing builders and http4s metrics adapter.

## Choose an API

| Existing use | 0.20 option | What to review |
| --- | --- | --- |
| `ClientMiddleware` / `ServerMiddleware` | Keep tracing-only builders, or adopt `ClientInstrumentation` / `ServerInstrumentation` | Recording includes the response body and its cleanup |
| `OtelMetrics` with http4s Metrics middleware | Keep the adapter, or adopt `ClientMetrics` / `ServerMetrics` | Different metric names, labels and recording times |
| Separate tracing and metrics wrappers | Use combined instrumentation | Metrics are recorded before the HTTP span ends |

The existing `metrics`, `trace-client` and `trace-server` modules are still available. The new
`metrics-client`, `metrics-server`, `client` and `server` modules provide separate APIs.

When switching to combined instrumentation, remove the old metrics and tracing wrappers so each
request is recorded once.

## Providers and context

The new middleware takes providers as contextual parameters, supplied with `given` in Scala 3.
Metrics-only middleware needs `MeterProvider` and `ContextManager`. The combined middleware also
needs `TracerProvider`. Use providers from the same telemetry instance. Configure the backend
separately from the middleware. See the complete [Getting started](getting-started.md) example.

The shared `ErrorClassifier`, `PerRequestFilter` and `PerRequestConfig` types now live under
`org.http4s.otel4s.middleware`. The old tracing-package types are still available for
compatibility. Use the shared imports in new code.

## Tracing attributes and redaction

Client spans now use the response's HTTP protocol version. The destination address comes from
the request URI's host, with the `Host` header as a fallback. The destination port comes from
the URI, or from `Host` if the URI has no authority. A known URI scheme supplies a default port
before connection metadata is used. If you use relative request URIs, check dashboards that
group spans by destination.

Both OpenTelemetry span data providers now have `withKnownMethods`. Metrics configs have the same
setting. Include your custom methods if you want them to appear by name instead of `HTTP` in span
names or `_OTHER` in method attributes. Method matching is case-sensitive.

`QueryRedactor.RedactSensitive` now masks values for the default sensitive query keys. If you use
this redactor, those values will no longer appear in URL attributes. Extend
`DefaultSensitiveQueryParameters` with application-specific keys such as `access_token`.
Overriding `sensitiveQueryParameters` replaces the default set. See
[URL and header redaction](tracing.md#url-and-header-redaction) for an example.

## Metrics and dashboards

The `OtelMetrics` adapter records metrics through http4s `MetricsOps` callbacks. The new metrics
middleware records HTTP duration, active requests and body sizes. Some old series have no direct
replacement, including separate header and total timings, abnormal termination counts and series
grouped by the old classifier.

Update dashboard queries and alerts to use the [new metrics](metrics.md). Duration is measured
in seconds, body sizes in bytes and active requests as a count. Duration uses one measurement
per completed exchange, without the old header/total phase label. Body processing and cleanup
can make it longer than a headers-only measurement.

Compare the old and new label sets before updating queries. Route and URL templates replace
arbitrary classifier values. The new middleware does not emit the old abnormal-termination
instrument. Exceptions and error responses add `error.type` to duration and available body-size
measurements instead.

Duration is enabled by default. Enable active requests and body sizes if you need them. Unknown
HTTP methods use `_OTHER` as their label value. Use route and URL templates such as
`/users/{id}` to avoid creating a separate metric series for each ID.

## Streaming behavior

The old server tracing wrapper ends its span when the app returns a response. The combined
server middleware keeps recording until the response body finishes, including cleanup. Wrap the
final app, after routing, error handlers and middleware that changes the response.

The old client tracing wrapper ends its span when the response resource is released. The
combined client middleware can finish earlier, when the body finishes. If the body is unread, it
finishes when the response is released. In both cases, recording finishes before backend
resource cleanup. Caller work inside `use` is no longer necessarily part of the HTTP span.

If a server response body never runs, the middleware cannot finish recording. Its span stays
open, no duration or sizes are recorded, and the active-request count stays incremented if
enabled. Check that your middleware stack runs empty, HEAD and replaced response bodies. See
[Streaming lifecycle](streaming-lifecycle.md).

## Before deploying

Compare spans and measurements for typical requests, streamed responses, failures and
cancellation. Check that the middleware uses the right providers, filters and redactors. If you
use trace sampling or exemplars, check those too. Verify the new dashboard queries before
removing the old ones.
