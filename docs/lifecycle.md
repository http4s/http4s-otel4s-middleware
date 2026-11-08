# Lifecycle explorer

Step through an HTTP exchange to see how spans, metrics and propagation change together. Choose
a client or server scenario, then use **Next event**. Select a span, metric or HTTP message for
details. This is a simulation. Times and IDs are examples. Body sizes come from the example’s
UTF-8 bytes.

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
  <div id="lifecycle-metrics-overview" aria-label="Metrics. Select a row or measurement to inspect."></div>
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

Reset clears the measurements. All plots use the same time axis. A histogram point appears when
a measurement is recorded. The body preview shows bytes as each chunk runs.

<details class="lc-disclosure"><summary>Simulation assumptions</summary>

The example uses W3C Trace Context and Baggage propagation, sampled spans and an SDK that keeps
exemplars. Active requests and response body sizes are enabled. In a real application, your
backend controls when data is exported and whether exemplars are kept. Open spans may not appear
in your trace viewer yet.

The `/widgets/{id}` template is configured for span names and metric labels. The URL redactor
adds `access_token` to the sensitive query keys because the default list does not include it.
Header capture is disabled by default and uses the default header redactor when enabled. Body
contents and baggage are not automatically added to span attributes.

The simulated server has no existing span context. Metrics-only middleware can use a context
supplied by other code, but does not read context from incoming request headers.

Each chunk is a piece of the body stream. It does not represent an HTTP chunked-transfer frame.
The example splits the UTF-8 bytes of `é` across two chunks. The character appears when the
second chunk completes it.

</details>

See [Streaming lifecycle](streaming-lifecycle.md) for completion rules and middleware ordering.

<noscript>This explorer requires JavaScript. The <a href="streaming-lifecycle.html">streaming lifecycle reference</a> describes the completion rules.</noscript>
