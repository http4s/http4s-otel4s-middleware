# Migrating to 0.20

This guide describes the upcoming 0.20 APIs. Use a published 0.20 version when available, or a
snapshot containing these modules while evaluating them.

## Choose an API

| Existing use | 0.20 option | What to review |
| --- | --- | --- |
| `ClientMiddleware` / `ServerMiddleware` | Keep tracing-only builders, or adopt `ClientInstrumentation` / `ServerInstrumentation` | Combined instrumentation follows body completion |
| `OtelMetrics` with http4s Metrics middleware | Keep the adapter, or adopt `ClientMetrics` / `ServerMetrics` | New instrument names, attributes and completion boundaries |
| Separate tracing and metrics wrappers | Use combined instrumentation | A shared completion boundary records metrics before ending the HTTP span |

The existing `metrics`, `trace-client` and `trace-server` artifacts remain distinct from the new
`metrics-client`, `metrics-server`, `client` and `server` artifacts. This is not an artifact rename.
Replace old wrappers when adopting the combined middleware to avoid duplicate instrumentation.

## Provider and context wiring

New combined and standalone metrics factories take providers through contextual parameters. Supply
`MeterProvider`, `TracerProvider` where required, and `ContextManager` from the same telemetry
instance. Configuration values contain policies, not backend instances. See the complete
[Getting started](getting-started.md) example.

The shared `ErrorClassifier`, `PerRequestFilter` and `PerRequestConfig` types now live under
`org.http4s.otel4s.middleware`. Existing tracing-package compatibility types remain, but new
code should use the shared imports.

## Metrics and dashboards

The `OtelMetrics` adapter follows http4s `MetricsOps` callbacks. The new dedicated middleware uses
HTTP duration, active-request and optional body-size instruments. It does not reproduce every
legacy header/total timing, abnormal-termination or classifier-based series. Update dashboard
queries and alerts against the [new instrument table](metrics.md), including units and labels.

Duration is enabled by default. Active requests and body sizes require explicit opt-ins. Unknown
methods normalize to `_OTHER`; use route and URL templates for bounded cardinality.

## Streaming behavior

The old server tracing wrapper ends after app evaluation produces a response. Combined server
instrumentation continues through response-body termination and cleanup. Apply it once to the
final app so outer transformations cannot discard its body unnoticed.

The old client tracing wrapper follows response resource scope. Combined client instrumentation
completes at body termination or unread response release, before backend release finalizers.
Review code that assumed caller work inside `use` was part of HTTP duration.

A successful server response whose body never starts has no completion signal. Its span stays
open, duration and sizes are not recorded, and enabled active requests remain incremented.
Test middleware ordering and the server backend with empty, HEAD and replaced responses.
See [Lifecycle and limitations](lifecycle.md).

## Before deploying

Compare a representative request's spans and measurements, including streaming, failures and
cancellation. Verify provider wiring, filters, redaction, sampling and exemplar configuration.
Run old and new dashboards against the resulting telemetry before removing legacy queries.
