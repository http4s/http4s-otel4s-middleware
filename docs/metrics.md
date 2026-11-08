# Metrics

The `metrics-client` and `metrics-server` modules record the HTTP metrics defined by the
OpenTelemetry semantic conventions. The combined `client` and `server` modules record the same
metrics.

## Instruments and defaults

| Instrument | Unit | Default | Recorded when |
| --- | --- | --- | --- |
| `http.client.request.duration` | seconds | Enabled | Client exchange completes |
| `http.server.request.duration` | seconds | Enabled | Server exchange completes |
| `http.client.active_requests` | requests | Disabled | Increment when acquisition starts, decrement when recording finishes |
| `http.server.active_requests` | requests | Disabled | Increment before the app runs, decrement when recording finishes |
| `http.client.request.body.size` / `http.server.request.body.size` | bytes | Disabled | Exchange completes and a size is available |
| `http.client.response.body.size` / `http.server.response.body.size` | bytes | Disabled | Exchange completes and a size is available |

Active requests and body sizes are disabled by default. Duration includes body processing and
cleanup, as described in [Streaming lifecycle](streaming-lifecycle.md). The standard duration
bucket boundaries, in seconds, are `0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1,
2.5, 5, 7.5, 10`.

```scala mdoc:silent
import org.http4s.otel4s.middleware.metrics.BodySizeConfig
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.typelevel.otel4s.metrics.BucketBoundaries

val config = ClientMetricsConfig.default
  .optIntoActiveRequests
  .optIntoRequestBodySize(BodySizeConfig.observe)
  .optIntoResponseBodySize(BodySizeConfig.observe)
  .withResponseDurationSecondsHistogramBuckets(
    BucketBoundaries(0.01, 0.05, 0.1, 0.5, 1.0, 5.0)
  )
```

The server config has the same methods for enabling metrics and setting duration buckets. The
backend may use different histogram buckets. Configure SDK views or aggregation if needed to
make it use your chosen boundaries. The backend can also disable individual instruments.

## Body sizes

Choose how to measure request and response bodies separately:

| Strategy | Meaning | Incomplete or unread body |
| --- | --- | --- |
| `BodySizeConfig.contentLength` | Declared `Content-Length` | Records the declared length if present |
| `BodySizeConfig.observe` | Bytes from the first body evaluation | Incomplete observations are omitted |

Calling `optIntoRequestBodySize` or `optIntoResponseBodySize` without an argument uses
`BodySizeConfig.contentLength`.

Observed sizes count bytes passing through the instrumented stream. They are not a wire-size
measurement and do not include HTTP framing or TLS overhead. Compression placement affects the
bytes seen by the observer. Observation does not read or buffer the body on its own.

Responses that cannot carry a body, such as HEAD and 204, have a response size of zero
regardless of the declared length. The server still needs to run the response body before it can
finish recording. To set custom size buckets, pass them to
`BodySizeConfig.contentLength(buckets)` or `BodySizeConfig.observe(buckets)` using bytes as the
unit.

## Metric labels

Use `withAttributes` for shared labels and `withRequestAttributes` for request-derived labels.
Request labels override shared labels. The standard HTTP attributes override both. Avoid IDs,
raw URLs and query values in labels. Each distinct combination of label values creates another
metric series.

Client metrics include the destination address and port when available. To add a URL template,
configure a `UriTemplateClassifier`. To add the URL scheme, use `optIntoUrlScheme`.

Server metrics use a `RouteClassifier` for route templates. Server address and port must be
enabled separately. If a classifier does not match, the template attribute is omitted. The
library does not use the raw path as a fallback.

Only known HTTP methods appear as label values. Other methods become `_OTHER`. Matching is
case-sensitive. Pass an extended `TypedAttributes.DefaultKnownMethods` set to `withKnownMethods`
if your service uses more methods. An empty set maps every method to `_OTHER`.

## Link measurements to traces

An exemplar links a metric measurement to a trace. The combined middleware records metrics in
the HTTP context before ending the span, so a backend that keeps exemplars can link them to that
span. Whether the link appears depends on sampling, SDK settings and backend support.

Metrics-only middleware uses the context that was current when the request started. It does not
create a span.

For the older http4s `MetricsOps` adapter in the `metrics` artifact, see [Migrating to
0.20](migration.md). It records different metrics at different points in a request.
