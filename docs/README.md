# http4s-otel4s-middleware

OpenTelemetry metrics and tracing for http4s clients and servers, built on
[otel4s](https://typelevel.org/otel4s/). Record spans and metrics for streaming requests and
responses. Request and response body effects keep the HTTP context, even when they run later.

## Start here

1. [Getting started](getting-started.md): add dependencies and send your first request.
2. [Client instrumentation](client.md) or [Server instrumentation](server.md): wrap your client or app.
3. [Streaming lifecycle](streaming-lifecycle.md): learn when requests finish and where to place the middleware.

## Choose a module

| Need | Artifact suffix | Entry point |
| --- | --- | --- |
| Client metrics and tracing | `client` | `ClientInstrumentation` |
| Server metrics and tracing | `server` | `ServerInstrumentation` |
| Client metrics only | `metrics-client` | `ClientMetrics` |
| Server metrics only | `metrics-server` | `ServerMetrics` |
| Client tracing only | `trace-client` | `ClientMiddleware` |
| Server tracing only | `trace-server` | `ServerMiddleware` |

All artifact names start with `http4s-otel4s-middleware-`. Scala 2.13 and Scala 3 are supported
on JVM, Scala.js and Scala Native. Choose a telemetry backend that supports your platform. The
OtelJava example in this guide runs on the JVM.

## Explore

- [Metrics](metrics.md): available metrics, body sizes, labels and links to traces.
- [Tracing and propagation](tracing.md): context, span attributes and redaction.
- [Filtering and errors](filtering.md): choose which requests to record and which responses count as errors.
- [Lifecycle explorer](lifecycle.md): watch spans, metrics and headers change one step at a time.

Source and API reference: [GitHub](https://github.com/http4s/http4s-otel4s-middleware) ·
[API reference](https://javadoc.io/doc/org.http4s/http4s-otel4s-middleware-client_3).
