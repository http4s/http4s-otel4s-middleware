# Tracing and propagation

## Span names and attributes

`ClientSpanDataProvider.openTelemetry` and `ServerSpanDataProvider.openTelemetry` implement the
HTTP semantic-convention span data. Client spans use kind `Client`; server spans use kind `Server`.
Known request methods form span names, with a classified URL template or route when available.
Unknown methods use `HTTP` in span names and `_OTHER` in the normalized method attribute.

Providers supply names, request attributes, response attributes and exception attributes.
Implement `SpanDataProvider` for custom behavior, or use the OpenTelemetry provider's builder
methods to enable additional attributes. Header capture is opt-in.

## URL and header redaction

Client providers require a `UriRedactor`; server providers require a path-and-query redactor.
Choose these deliberately because span attributes can contain application data. For example:

```scala mdoc:silent
import org.http4s.otel4s.middleware.trace.client.{ClientSpanDataProvider, UriRedactor}
import org.http4s.otel4s.middleware.trace.redact.{HeaderRedactor, QueryRedactor}

val redactor = new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive {
  override def sensitiveQueryParameters: Set[String] =
    QueryRedactor.DefaultSensitiveQueryParameters ++ Set("access_token", "api_key")
}
val spanDataProvider = ClientSpanDataProvider.openTelemetry(redactor)
  .optIntoHttpRequestHeaders(HeaderRedactor.default)
```

`OnlyRedactUserInfo` masks URI credentials but retains paths, query values and fragments.
Mixing in `RedactSensitive` additionally masks values for selected query keys. Matching is
**case-sensitive**. Overriding `sensitiveQueryParameters` replaces the defaults, so extend the
set when you want to preserve them. This is a selected-key policy, not a guarantee that every
secret is removed. Review path segments, fragments and application-specific parameters too.

Server providers accept a redactor such as
`new PathRedactor.NeverRedact with QueryRedactor.RedactSensitive {}`. For paths containing
sensitive data, supply your own path policy. Redaction changes telemetry attributes; it does not
rewrite the HTTP request sent to the service.

## Context across a request

For combined instrumentation, the flow is:

```text
caller span
  └─ client HTTP span ── propagated request headers ──► server HTTP span
       request/response body effects                    app/response body effects
```

Client request propagation is enabled by default for traced requests. The server extracts the
incoming context, including baggage. Configure propagators on your otel4s backend; the middleware
uses that configuration rather than implementing a separate propagation format.

Request and response body effects retain the captured HTTP context even when evaluated later.
Code consuming a body retains its own context. Supply the meter provider, tracer provider and
context manager from the same telemetry instance so recording and propagation agree.

Filtering tracing off also bypasses the combined middleware's tracing propagation for that
request. Disabling propagation alone does not disable the local span. Metrics have an independent
filter. See [Filtering and errors](filtering.md).

## Response propagation

Server response propagation is disabled by default. Enable it selectively with
`ServerTracingConfig.withReversePropagationFilter` to inject context into response headers.
It is separate from extraction of incoming request context. Existing unrelated headers are
preserved.

## Completion

Combined instrumentation ends its span after recording metrics at the shared exchange completion
boundary. Existing tracing-only builders have different boundaries: client spans follow the
response resource scope, and server spans follow app evaluation through response creation.
See [Lifecycle and limitations](lifecycle.md) before choosing an API.
