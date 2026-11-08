# Tracing and propagation

## Span names and attributes

`ClientSpanDataProvider.openTelemetry` and `ServerSpanDataProvider.openTelemetry` provide span
names and attributes that follow the OpenTelemetry HTTP semantic conventions. Client spans have
kind `Client`. Server spans have kind `Server`.

Span names contain a known request method and a URL template or route when configured, such as
`GET /users/{id}`. Unknown methods use `HTTP` in span names and `_OTHER` in the method
attribute.

Providers supply names, request attributes, response attributes and exception attributes.
Implement `SpanDataProvider` for custom behavior, or use the OpenTelemetry provider's builder
methods to add attributes. Header capture is disabled by default.

## URL and header redaction

A client provider needs a `UriRedactor`. A server provider needs a path and query redactor.
These control which parts of a URL appear in span attributes. For example:

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

`OnlyRedactUserInfo` masks URI credentials but keeps paths, query values and fragments. Adding
`RedactSensitive` also masks values for selected query keys. Matching is **case-sensitive**.
Overriding `sensitiveQueryParameters` replaces the defaults, so extend the set when you want to
preserve them. Only the selected keys are masked. Check whether paths, fragments or other query
parameters contain sensitive values too.

Server providers accept a redactor such as `new PathRedactor.NeverRedact with
QueryRedactor.RedactSensitive {}`. If paths contain sensitive data, provide a redactor that
masks it. Redaction changes the span attributes. The HTTP request sent to the service stays the
same.

## Context across a request

With combined client and server instrumentation, context flows like this:

```text
caller span
  └─ client HTTP span ── propagated request headers ──► server HTTP span
       request/response body effects                    app/response body effects
```

Client request propagation is enabled by default for traced requests. The server extracts the
incoming context, including baggage. Configure propagators on your otel4s backend. The
middleware uses those propagators to read and write context headers.

Request and response body effects keep the HTTP context even when they run later. The code that
consumes the body keeps its own context. Use the meter provider, tracer provider and context
manager from the same telemetry instance.

If the tracing filter disables tracing for a request, the combined middleware also skips context
injection on the client and context extraction on the server. Disabling only the propagation
filter still allows the client to create a local span. Metrics use a separate filter. See
[Filtering and errors](filtering.md).

## Response propagation

Server response propagation is disabled by default. Use
`ServerTracingConfig.withReversePropagationFilter` to choose which responses get context
headers. This setting does not affect context extraction from incoming requests. Other response
headers stay unchanged.

## When spans end

The combined middleware ends its span after recording metrics, once the body finishes or an
unread client response is released.

The tracing-only builders end spans at different points. Client spans end when the response
resource is released. Server spans end when the app returns a response. See [Streaming
lifecycle](streaming-lifecycle.md) before choosing an API.
