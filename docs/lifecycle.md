# Lifecycle and limitations

## Lifecycle explorer

Choose a client or server scenario, then use **Next event** to see how spans, metrics and
propagation change during an HTTP exchange. Select a span, metric or HTTP message for details.
This is a simulation: timestamps and IDs are illustrative; byte counts come from the actual UTF-8 fixtures.

<div id="lifecycle-widget" class="lifecycle-explorer" hidden>
  <div class="lifecycle-controls">
    <label>Side<select id="lifecycle-side"><option value="client">Client</option><option value="server">Server</option></select></label>
    <label>Outcome<select id="lifecycle-scenario"><option value="success">Body completes</option><option value="body-error">Body fails</option><option value="body-cancel">Body canceled</option><option value="unread">Body never starts</option><option value="app-error">Acquisition / app fails</option><option value="app-cancel">Acquisition / app canceled</option></select></label>
  </div>
  <details class="lc-disclosure lc-config"><summary>Configure scenario</summary>
    <div class="lifecycle-controls">
      <label>Method<select id="lifecycle-method"><option value="GET">GET</option><option value="POST">POST</option><option value="HEAD">HEAD</option></select></label>
      <label>Response status<select id="lifecycle-status"><option value="200">200 OK</option><option value="204">204 No Content</option><option value="404">404 Not Found</option><option value="503">503 Unavailable</option></select></label>
      <label>Response-size source<select id="lifecycle-bodySize"><option value="observe">Observe stream</option><option value="contentLength">Content-Length</option></select></label>
    </div>
    <fieldset class="lc-policies"><legend>Request policies</legend>
      <label><input id="lifecycle-metrics" type="checkbox" checked> Metrics enabled</label>
      <label><input id="lifecycle-tracing" type="checkbox" checked> Tracing enabled</label>
      <label><input id="lifecycle-propagate" type="checkbox" checked> Client request propagation</label>
      <label><input id="lifecycle-parent" type="checkbox" checked> <span id="lifecycle-parent-label">Caller context + baggage</span></label>
      <label><input id="lifecycle-reverse" type="checkbox"> Server response propagation</label>
      <label><input id="lifecycle-captureHeaders" type="checkbox"> Capture allowed headers as span attributes</label>
    </fieldset>
  </details>
  <div class="lifecycle-controls lc-transport"><button type="button" id="lifecycle-reset">Reset</button><button type="button" id="lifecycle-back">Previous</button><button type="button" id="lifecycle-next">Next event →</button><span class="lc-toolbar-spacer"></span><button type="button" id="lifecycle-request">Inspect request</button><button type="button" id="lifecycle-response">Inspect response</button></div>
  <details class="lc-disclosure" open><summary>Exchange events · jump to any step</summary><div id="lifecycle-steps"></div></details>
  <div id="lifecycle-state" role="status" aria-live="polite" aria-atomic="true"></div>
  <p id="lifecycle-explanation" class="lc-muted"></p>
  <div id="lifecycle-metrics-overview" aria-label="Metrics; select a row or measurement to inspect"></div>
  <h3 class="lc-overview-title">Trace waterfall</h3>
  <div id="lifecycle-trace"></div>
  <div class="lc-signal-row lc-axis-row"><span>Simulated time</span><div id="lifecycle-time-axis"></div></div>
  <p class="lc-muted">Dashed: external parent. Accent: open span. Green: ended. Red: ended with error. Select a row to expand or collapse its details.</p>
  <div id="lifecycle-inspector-home">
  <section id="lifecycle-inspector" class="lc-inspector" aria-labelledby="lifecycle-inspector-title" hidden>
    <div class="lc-inspector-heading"><h3 id="lifecycle-inspector-title"></h3><button type="button" id="lifecycle-inspector-close" aria-label="Close inspector">Close</button></div>
    <p id="lifecycle-field-note" class="lc-field-note" role="status" aria-live="polite"></p>
    <div id="lifecycle-inspector-content"></div>
  </section>
  </div>
</div>

Try these comparisons:

- Open **Inspect request**, then step through **Body completes**: each response chunk reveals its
  exact byte slice; duration and size record once at completion, followed by span end at the same timestamp.
- Select **Server + Body never starts**: the active count stays at one and no histogram sample appears.
- Compare **404** on client and server: default client classification marks it as an error; server classification does not.
- Turn **tracing off**: metrics continue, but client injection and server context extraction stop.
- Turn **client request propagation off**: its local span remains, but no new request context is injected.
- Enable **server response propagation**: response headers contain the server span context once headers exist.
- Compare **Observe stream** and **Content-Length** with an unread or interrupted body: incomplete observed sizes are omitted;
  declared length can still be recorded at completion.
- Choose **POST** under **Configure scenario** to step through request-body chunks before response headers.
- Click a metric point to inspect its attributes and buckets; follow its exemplar to the associated span.
- Select a URL or header in the inspector to highlight related telemetry. The demo query value is redacted in URL attributes.

### How to read the simulation

Each reset starts with zero measurements. The active-request plot is a step chart; duration and
response-size plots show individual recorded measurements, in milliseconds and bytes respectively.
The duration instrument itself uses seconds. All three plots share the same simulated time axis.
With JavaScript disabled, the completion table below provides the same rules.

The body previews show only evaluated chunks. The fixtures containing `café` intentionally split the UTF-8
encoding of `é` across chunks; its character appears when the next chunk completes it. Byte ranges and hex values
are available under **exact bytes**. **Remaining example body** shows only data still awaiting evaluation
and disappears once all bytes have been evaluated. Destination badges identify where shared attributes
are recorded. Chunks are stream evaluations, not
HTTP chunked-transfer frames. HEAD can declare a representation length while evaluating zero body
bytes; 204 has no Content-Length header or payload.

The example assumes W3C Trace Context and Baggage propagators, sampled spans, active requests and
response-body sizes opted in, and an SDK that retains exemplars. Real exemplar retention and
export timing depend on your backend. A span shown as open has not ended and may not appear in a
real trace backend yet. HTTP span naming and bounded metric labels use a configured `/widgets/{id}` classifier.
The URL redactor explicitly includes `access_token`; default sensitive-query keys alone do not
include it. Allowed header capture is opt-in and uses the default header redactor. Body contents
and baggage do not become span attributes automatically. The simulated server backend has
no ambient span. Metrics-only middleware can capture an ambient context supplied elsewhere, but
does not itself extract incoming request headers.

## Completion boundaries

HTTP response creation and body completion are separate events in http4s. The rules below apply
to **combined instrumentation and standalone metrics**. Existing tracing-only builders use
response resource scope on the client and app evaluation on the server.

| Scenario | Client | Server |
| --- | --- | --- |
| Acquisition / app fails | Completes with error | Completes with error |
| Acquisition / app canceled | Completes; cancellation alone is neutral | Completes; cancellation alone is neutral |
| Body finishes | Completes, including body cleanup | Completes, including body cleanup |
| Body fails | Completes with observed exception | Completes with observed exception |
| Started body is canceled | Completes; cancellation alone is neutral | Completes; cancellation alone is neutral |
| Response body is never started | Completes when response resource is released | Remains incomplete |

At completion, duration and available body sizes are recorded, active requests are decremented
if enabled, then the combined middleware ends the HTTP span. Completion is recorded once.
A status already classified as an error remains an error even if body termination is neutral.

Client completion precedes backend resource release finalizers. Caller code errors do not become
HTTP failures. Server completion includes response-body cleanup, but does not prove that bytes
reached the remote client.

## Bodies that never start

A server `HttpApp` returns a response without owning a response resource that it can finalize
later. If nobody evaluates that response's body, there is no terminal signal. Even an empty
stream must be run. HEAD and no-content responses obey this rule too, although their response
size may be known to be zero.

Apply server instrumentation to the **final app**. An outer middleware that replaces the response
without running its original body can strand its telemetry. Include error recovery, routing and
response transformations inside that boundary when you want to measure their resulting response.

On the client, always release the response resource. Unread responses are completed at release,
but observed body sizes are omitted when observation is incomplete. Declared Content-Length can
still be recorded. Do not leak resources to keep spans open.

## Context and errors

Request/body source effects retain the HTTP context captured for the exchange. The caller or
backend consuming a stream retains its own context. Exceptions observed at the instrumented
boundary take precedence over response-status errors. Cancellation alone is neutral.

A server-side source exception and the corresponding client-side transport outcome can differ:
the backend may surface an error, truncation or normal completion. The middleware records what
it observes; client and server spans need not have identical error classification.

## Verify in your application

Exercise a streamed response, a body error, cancellation, an unread client response and an empty
server response with your actual backend and middleware stack. If server spans remain open or
active requests accumulate, check whether bodies are discarded outside the instrumentation.
