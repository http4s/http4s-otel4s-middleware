# http4s-otel4s-middleware

OpenTelemetry metrics and tracing for http4s clients and servers, built on
[otel4s](https://typelevel.org/otel4s/). Instrument streaming HTTP exchanges while preserving
context through request and response bodies.

This documentation describes the upcoming **0.20.0** APIs. New combined instrumentation and
standalone metrics modules are not available in 0.19.0. See [Migrating to 0.20](migration.md)
if you already use the library.

## Start here

1. [Getting started](getting-started.md): choose dependencies and send your first instrumented request.
2. [Client instrumentation](client.md) or [Server instrumentation](server.md): integrate with your application.
3. [Lifecycle and limitations](lifecycle.md): understand streaming completion and middleware ordering.

## Choose your instrumentation

| Need | Artifact suffix | Entry point |
| --- | --- | --- |
| Client metrics and tracing | `client` | `ClientInstrumentation` |
| Server metrics and tracing | `server` | `ServerInstrumentation` |
| Client metrics only | `metrics-client` | `ClientMetrics` |
| Server metrics only | `metrics-server` | `ServerMetrics` |
| Client tracing only | `trace-client` | `ClientMiddleware` |
| Server tracing only | `trace-server` | `ServerMiddleware` |

All artifact names start with `http4s-otel4s-middleware-`. Scala 2.13 and Scala 3 are supported
on JVM, Scala.js and Scala Native. Backend availability depends on the telemetry implementation;
the OtelJava example in this guide is JVM-only.

## Explore

- [Metrics](metrics.md): defaults, body sizes, cardinality and exemplars.
- [Tracing and propagation](tracing.md): context, span attributes and redaction.
- [Filtering and errors](filtering.md): request policies and error classification.
- [Lifecycle explorer](lifecycle.md#lifecycle-explorer): step through completion scenarios.

Source and API reference: [GitHub](https://github.com/http4s/http4s-otel4s-middleware) ·
[Published API docs](https://javadoc.io/doc/org.http4s/http4s-otel4s-middleware-client_3)
(new module API docs become available after the first release).
