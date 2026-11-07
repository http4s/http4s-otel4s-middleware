# Metrics

The `metrics-client` and `metrics-server` modules record HTTP semantic-convention metrics.
The combined `client` and `server` modules use the same recorders.

## Instruments and defaults

| Instrument | Unit | Default | Recorded when |
| --- | --- | --- | --- |
| `http.client.request.duration` | seconds | Enabled | Client exchange completes |
| `http.server.request.duration` | seconds | Enabled | Server exchange completes |
| `http.client.active_requests` | requests | Disabled | Increment at acquisition; decrement at completion |
| `http.server.active_requests` | requests | Disabled | Increment before app execution; decrement at completion |
| `http.client.request.body.size` / `http.server.request.body.size` | bytes | Disabled | Exchange completes and a size is available |
| `http.client.response.body.size` / `http.server.response.body.size` | bytes | Disabled | Exchange completes and a size is available |

Active requests and body-size instruments are opt-ins. Duration covers the exchange through its
[completion boundary](lifecycle.md), rather than just time to headers. The standard duration
bucket boundaries, in seconds, are `0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75,
1, 2.5, 5, 7.5, 10`.

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

The server config has corresponding opt-ins and duration bucket configuration. Histogram bucket
settings are advice to the backend; configure SDK views or aggregation where your backend requires
it. Backend instrument enablement also controls what is recorded.

## Body sizes

Choose the source separately for request and response:

| Strategy | Meaning | Incomplete or unread body |
| --- | --- | --- |
| `BodySizeConfig.contentLength` | Declared `Content-Length`; this is also the no-argument opt-in | Declared length can still be recorded; missing lengths are omitted |
| `BodySizeConfig.observe` | Bytes from the first body evaluation | Incomplete observations are omitted |

Observed sizes count bytes flowing through the instrumented stream. They are not a wire-size
measurement and do not include HTTP framing or TLS overhead. Compression placement affects the
bytes seen by the observer. Observation does not force or buffer body evaluation.

Responses that cannot carry a body, such as HEAD and 204, have a protocol-known zero response
size. That does not change the server completion requirement: its response body must still be
run. Custom size buckets are supplied to `BodySizeConfig.contentLength(buckets)` or
`BodySizeConfig.observe(buckets)` and use bytes.

## Attributes and cardinality

Use `withAttributes` for shared labels and `withRequestAttributes` for request-derived labels.
Request labels override shared labels; generated semantic attributes take precedence over both.
Avoid identifiers, raw URLs and arbitrary query values in metric labels.

Client metrics include destination attributes when available. URL scheme and URL template are
opt-ins; configure a `UriTemplateClassifier` and use `optIntoUrlScheme` as needed. Server metrics
accept a `RouteClassifier`; server address and port are separate opt-ins. Unclassified templates
are omitted, without a raw-path fallback.

Known methods use a bounded set. Unknown methods become `_OTHER`; matching is case-sensitive.
Extend `TypedAttributes.DefaultKnownMethods` with `withKnownMethods` if your service uses more
methods. An empty known-method set maps every method to `_OTHER`.

## Exemplars

Combined instrumentation records metrics in the HTTP context before ending the span. A backend
configured to retain exemplars can therefore associate measurements with the HTTP span. Sampling,
exemplar filtering, storage and display belong to the SDK and observability backend; a measurement
does not guarantee a visible exemplar. Standalone metrics use the captured ambient context.

For the older http4s `MetricsOps` adapter in the `metrics` artifact, see
[Migrating to 0.20](migration.md); its instruments and boundaries differ.
