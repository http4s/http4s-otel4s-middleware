/* Deterministic illustration of combined instrumentation, not an SDK or HTTP transport. */
(function () {
  "use strict";
  const ids = { trace: "4bf92f3577b34da6a3ce929d0e0e4736", rootTrace: "7a920f3577b34da6a3ce929d0e0e4700",
    parent: "1111111111111111", client: "2222222222222222", server: "3333333333333333" };
  const boundaries = [0.005, 0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 0.75, 1, 2.5, 5, 7.5, 10, Infinity];
  const byteBoundaries = [0, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, Infinity];
  const encoder = new TextEncoder();
  function options(value) {
    return Object.assign({ side: "client", scenario: "success", status: 200, method: "GET", metrics: true,
      tracing: true, propagate: true, parent: true, reverse: false, bodySize: "observe", captureHeaders: false }, value);
  }
  function body(text, count) {
    const bytes = Array.from(encoder.encode(text));
    if (!bytes.length) { return { bytes, chunks: [] }; }
    // Deliberately split the UTF-8 encoding of é across two chunks when present.
    const unicode = bytes.indexOf(0xc3);
    const first = unicode >= 0 ? unicode + 1 : Math.ceil(bytes.length / count);
    const cuts = count === 2 ? [first] : [first, first + Math.ceil((bytes.length - first) / 2)];
    const offsets = [0, ...cuts.filter(n => n > 0 && n < bytes.length), bytes.length];
    const chunks = offsets.slice(1).map((end, index) => ({ index, start: offsets[index], end,
      bytes: bytes.slice(offsets[index], end) }));
    return { bytes, chunks };
  }
  function fixture(value) {
    const config = options(value);
    const requestBody = body(config.method === "POST" ? '{"name":"café"}\n' : "", 2);
    const reason = { 200: "OK", 204: "No Content", 404: "Not Found", 503: "Service Unavailable" }[config.status];
    const responseText = config.status === 404 ? '{"error":"not found"}\n{"id":42}\n{"retry":false}\n'
      : config.status === 503 ? '{"error":"unavailable"}\n{"id":42}\n{"retry":true}\n'
        : '{"id":42,"name":"café"}\n{"id":43,"name":"tea"}\n{"id":44,"name":"water"}\n';
    const represented = body(responseText, 3);
    const noBody = config.method === "HEAD" || config.status === 204;
    const responseBody = noBody ? body("", 3) : represented;
    const declaredLength = config.status === 204 ? null : represented.bytes.length;
    const requestHeaders = [["Host", "api.example.test"], ["Accept", "application/x-ndjson"], ["X-Request-ID", "demo-42"]];
    if (requestBody.bytes.length) {
      requestHeaders.push(["Content-Type", "application/json"], ["Content-Length", String(requestBody.bytes.length)]);
    }
    const responseHeaders = [["X-Request-ID", "demo-42"]];
    if (declaredLength !== null) {
      responseHeaders.unshift(["Content-Type", "application/x-ndjson"], ["Content-Length", String(declaredLength)]);
    }
    return { request: { method: config.method, scheme: "http", host: "api.example.test", port: 80,
      path: "/widgets/42", query: "access_token=demo-secret", template: "/widgets/{id}", protocol: "1.1",
      headers: requestHeaders, body: requestBody },
    response: { status: config.status, reason, protocol: "1.1", headers: responseHeaders,
      declaredLength, body: responseBody, noBody } };
  }
  function progress(payload, count) {
    const chunks = payload.chunks.slice(0, count);
    const bytes = chunks.flatMap(chunk => chunk.bytes);
    const decoder = new TextDecoder();
    const segments = chunks.map(chunk => decoder.decode(Uint8Array.from(chunk.bytes), { stream: true }));
    const text = segments.join("");
    return { chunks, bytes, text, segments, total: payload.bytes.length, pendingBytes: bytes.length - encoder.encode(text).length };
  }
  function remainder(payload, count) {
    const seen = progress(payload, count);
    const bytes = payload.bytes.slice(seen.bytes.length);
    const decoder = new TextDecoder();
    decoder.decode(Uint8Array.from(seen.bytes), { stream: true });
    return { bytes, text: decoder.decode(Uint8Array.from(bytes)), completesCharacter: seen.pendingBytes > 0 };
  }
  function groupAttributes(entries) {
    const groups = new Map();
    entries.forEach(entry => {
      const key = JSON.stringify([entry.key, entry.value, entry.source]);
      if (!groups.has(key)) { groups.set(key, Object.assign({}, entry, { destinations: [] })); }
      const grouped = groups.get(key);
      (entry.destinations || []).forEach(destination => {
        if (!grouped.destinations.includes(destination)) { grouped.destinations.push(destination); }
      });
    });
    return Array.from(groups.values());
  }
  function measurementState(data, event, metric) {
    if (!data.config.metrics) { return "Disabled by metrics filter"; }
    if (!data.started) { return "Not started"; }
    if (metric === "duration" && event.duration) { return "Recorded"; }
    if (metric === "size" && event.bodySize !== null) { return "Recorded"; }
    if (metric === "size" && !event.response) {
      return event.completed ? "Not recorded: no response" : "Awaiting response";
    }
    if (!event.completed) {
      if (event.response && !event.bodyStarted && event.time >= 80 && data.config.scenario === "unread") {
        return "Not recorded: body never started";
      }
      return event.bodyComplete ? "Awaiting body cleanup" : "Awaiting exchange completion";
    }
    if (data.config.scenario === "unread") { return "Not recorded: body was unread"; }
    return "Not recorded: body " + (data.config.scenario === "body-error" ? "failed" : "was canceled") + " before EOF";
  }
  // Names follow the Scala lifecycle suites; timestamps are illustrative. Chunk sizes come only from fixture bytes.
  function events(value) {
    const config = options(value);
    const messages = fixture(config);
    const client = config.side === "client";
    const result = [];
    let current = { time: 0, phase: "before", label: "Before request", span: "not started", active: 0,
      duration: false, error: false, errorType: null, response: false, requestBytes: 0, requestChunkCount: 0,
      bytes: 0, chunkCount: 0, requestComplete: false, bodyStarted: false, bodyComplete: false, completed: false, durationSeconds: 0, bodySize: null, chunk: null };
    const push = (label, time, explanation, update) => {
      current = Object.assign({}, current, { label, time, explanation, chunk: null }, update);
      result.push(current);
    };
    function consume(direction, index, time) {
      const payload = messages[direction].body;
      const chunk = payload.chunks[index];
      const seen = progress(payload, index + 1);
      const request = direction === "request";
      push((request ? "Request" : "Response") + " chunk " + (index + 1) + " · +" + chunk.bytes.length + " B", time,
        "Evaluate fixture bytes [" + chunk.start + ", " + chunk.end + ") in the HTTP source context. " +
        seen.bytes.length + "/" + payload.bytes.length + " body bytes have passed this boundary. " +
        "Partial UTF-8 characters wait for the next chunk; chunks do not record histogram samples.",
        Object.assign({ phase: request ? "request-body" : "body", chunk: { direction, index, start: chunk.start,
          end: chunk.end, bytes: chunk.bytes } }, request ? { requestBytes: seen.bytes.length, requestChunkCount: index + 1 }
          : { bytes: seen.bytes.length, chunkCount: index + 1, bodyStarted: true }));
    }
    push("Before request", 0, "A synthetic request is prepared. No HTTP exchange or body evaluation has started.");
    push(client ? "Start acquisition" : "Start app", 5,
      config.tracing ? "Start the HTTP span. Enabled metrics increment active requests independently."
        : "Tracing is filtered out. Enabled metrics still capture ambient context and increment active requests.",
      { phase: "start", span: config.tracing ? "open" : "disabled", active: config.metrics ? 1 : 0 });
    messages.request.body.chunks.forEach((_, index) => consume("request", index, 10 + index * 5));
    if (messages.request.body.bytes.length) {
      push("Request body reaches EOF", 18, "All request fixture bytes were evaluated. This example opts into response-size metrics only.", { phase: "request-eof", requestComplete: true });
    } else {
      push("Evaluate empty request body", 10, "The request fixture has no payload bytes. Evaluating its empty body reaches EOF.", { phase: "request-eof", requestComplete: true });
    }
    function complete(time, exception) {
      const statusError = current.response && (client ? config.status >= 400 : config.status >= 500);
      const errorType = exception || (statusError ? String(config.status) : null);
      const size = !current.response ? null : messages.response.noBody ? 0
        : config.bodySize === "contentLength" ? messages.response.declaredLength : current.bodyComplete ? current.bytes : null;
      push("Record metrics", time,
        config.metrics ? "Record duration and available size, then decrement active requests." +
          (config.tracing ? " The HTTP span has not ended yet." : " No HTTP span was created.")
          : "Metrics are filtered out: no instruments change. Span completion remains independent.",
        { phase: "metrics", completed: true, active: 0, duration: config.metrics,
          durationSeconds: (time - 5) / 1000, bodySize: config.metrics ? size : null, error: !!errorType, errorType });
      push(config.tracing ? "End HTTP span" : "Exchange complete", time,
        config.tracing ? "Add response/error attributes and end the span after metrics. Select its bar or a metric to inspect the recorded data."
          : "No HTTP span was created. Metrics-only recording uses the captured ambient context.",
        { phase: "end", span: config.tracing ? "ended" : "disabled" });
    }
    if (config.scenario === "app-error" || config.scenario === "app-cancel") {
      const failed = config.scenario === "app-error";
      push(failed ? "Acquisition / app fails" : "Acquisition / app canceled", 20,
        failed ? "An exception is observed before response headers."
          : "Cancellation alone is neutral; no response status exists to classify.", { phase: "terminal" });
      complete(20, failed ? "demo.AcquisitionFailure" : null);
      return result;
    }
    push("Response headers available", 25,
      "The response now exists. Content-Length describes the declared payload, not how many bytes have been evaluated.",
      { phase: "headers", response: true });
    if (config.scenario === "unread") {
      push(client ? "Release unread response" : "Discard body without starting", 80,
        client ? "Release supplies the completion signal without draining the body. No fixture chunks have been evaluated."
          : "No response body evaluation means no terminal signal. Its span and enabled active count remain open.", { phase: "terminal" });
      if (client) {
        complete(80, null);
        push("Backend release finalizers", 95, "Already completed. No second measurement or span end.");
      } else {
        push("Still waiting for completion", 200, "No duration or size sample exists. The body remains entirely unread.");
      }
      return result;
    }
    const interrupted = config.scenario === "body-error" || config.scenario === "body-cancel";
    if (messages.response.noBody) {
      push("Evaluate empty body", 50, "HEAD and 204 have no response payload. The empty stream must still run.", { phase: "body", bodyStarted: true });
    } else {
      messages.response.body.chunks.forEach((_, index) => {
        if (!interrupted || index < 2) { consume("response", index, [50, 85, 100][index]); }
      });
    }
    push(config.scenario === "body-error" ? "Body fails" : config.scenario === "body-cancel" ? "Body canceled" : "Body reaches EOF", 110,
      interrupted ? "The remaining fixture bytes are not evaluated. An incomplete observation is omitted; declared length can still be recorded."
        : "EOF confirms the first body evaluation is complete. Its actual byte count is now eligible for recording after cleanup.",
      { phase: "terminal", bodyComplete: !interrupted });
    complete(125, config.scenario === "body-error" ? "demo.BodyFailure" : null);
    if (client) { push("Backend release finalizers", 155, "HTTP recording is complete. Later release or caller failures are outside its duration."); }
    return result;
  }
  function snapshot(configValue, event) {
    const config = options(configValue);
    const messages = fixture(config);
    const started = event.phase !== "before";
    const local = config.side === "client" ? ids.client : ids.server;
    const traceId = config.parent ? ids.trace : ids.rootTrace;
    const parentId = config.side === "client" ? ids.parent : ids.client;
    const traced = started && config.tracing;
    const ambient = config.side === "client" && config.parent ? ids.parent : null;
    const scope = traced ? local : ambient;
    const baggage = config.parent && (config.side === "client" || config.tracing) ? "tenant=demo" : "none";
    const requestHeader = !started ? null : config.side === "server"
      ? config.parent ? "00-" + ids.trace + "-" + parentId + "-01" : null
      : traced && config.propagate ? "00-" + traceId + "-" + local + "-01" : null;
    const responseHeader = event.response && traced && config.side === "server" && config.reverse ? "00-" + traceId + "-" + local + "-01" : null;
    messages.request.headers = messages.request.headers.slice();
    messages.response.headers = messages.response.headers.slice();
    if (requestHeader) {
      messages.request.headers.push(["traceparent", requestHeader]);
      if (config.parent) { messages.request.headers.push(["baggage", "tenant=demo"]); }
    }
    if (responseHeader) {
      messages.response.headers.push(["traceparent", responseHeader]);
      if (baggage !== "none") { messages.response.headers.push(["baggage", baggage]); }
    }
    const row = (key, value, source) => ({ key, value, source });
    const spanAttributes = [];
    if (traced) {
      spanAttributes.push(row("http.request.method", config.method, "method"));
      const url = "http://" + messages.request.host + messages.request.path + "?access_token=REDACTED";
      if (config.side === "client") {
        spanAttributes.push(row("url.full", url, "target"), row("server.address", messages.request.host, "host"), row("server.port", 80, "host"));
      } else {
        spanAttributes.push(row("url.path", messages.request.path, "target"), row("url.query", "access_token=REDACTED", "target"),
          row("url.scheme", "http", "target"), row("http.route", messages.request.template, "target"),
          row("server.address", messages.request.host, "host"), row("server.port", 80, "host"), row("network.protocol.version", "1.1", "protocol"));
      }
      // Both span-name classifiers are configured; client url.template attribute remains an opt-in (off here).
      if (config.captureHeaders) {
        messages.request.headers.filter(([key]) => !["traceparent", "baggage"].includes(key)).forEach(([key, value]) => {
          spanAttributes.push(row("http.request.header." + key.toLowerCase(), [value], headerSource("request", key)));
        });
      }
      if (event.span === "ended") {
        if (event.response) {
          spanAttributes.push(row("http.response.status_code", config.status, "status"));
          if (config.side === "client") { spanAttributes.push(row("network.protocol.version", "1.1", "protocol")); }
          if (config.captureHeaders) {
            messages.response.headers.filter(([key]) => !["traceparent", "baggage"].includes(key)).forEach(([key, value]) => {
              spanAttributes.push(row("http.response.header." + key.toLowerCase(), [value], headerSource("response", key)));
            });
          }
        }
        if (event.errorType) { spanAttributes.push(row("error.type", event.errorType, event.errorType.startsWith("demo.") ? "exception" : "status")); }
      }
    }
    const startAttributes = [row("http.request.method", config.method, "method")];
    if (config.side === "client") {
      startAttributes.push(row("server.address", messages.request.host, "host"), row("server.port", 80, "host"),
        row("url.template", messages.request.template, "target"));
    } else { startAttributes.push(row("url.scheme", "http", "target")); }
    const terminalAttributes = startAttributes.slice();
    if (config.side === "server") { terminalAttributes.push(row("http.route", messages.request.template, "target"), row("network.protocol.version", "1.1", "protocol")); }
    if (event.response) {
      terminalAttributes.push(row("http.response.status_code", config.status, "status"));
      if (config.side === "client") { terminalAttributes.push(row("network.protocol.version", "1.1", "protocol")); }
    }
    if (event.errorType) { terminalAttributes.push(row("error.type", event.errorType, event.errorType.startsWith("demo.") ? "exception" : "status")); }
    return { config, messages, started, traced, local, traceId, scope, baggage, requestHeader, responseHeader,
      parentId: traced && config.parent ? parentId : null,
      spanStatus: traced ? event.span === "ended" && event.error ? "Error" : "Unset" : null,
      spanAttributes, startAttributes, terminalAttributes,
      count: event.duration ? 1 : 0, sum: event.duration ? event.durationSeconds : 0,
      sizeCount: event.bodySize === null ? 0 : 1, exemplarSpan: event.duration ? scope : null,
      buckets: boundaries.map(le => ({ le, count: event.duration && event.durationSeconds <= le ? 1 : 0 })),
      sizeBuckets: byteBoundaries.map(le => ({ le, count: event.bodySize !== null && event.bodySize <= le ? 1 : 0 })) };
  }
  function headerSource(direction, key) {
    const name = key.toLowerCase();
    if (name === "host") { return "host"; }
    if (name === "traceparent" || name === "baggage") { return name; }
    if (name === "content-length") { return direction + "-length"; }
    return direction + "-header-" + name;
  }
  if (typeof module !== "undefined" && module.exports) { module.exports = { events, snapshot, options, fixture, progress, remainder, groupAttributes, measurementState }; }
  if (typeof document === "undefined") { return; }
  function mount() {
    const root = document.getElementById("lifecycle-widget");
    if (!root) { return; }
    const byId = name => document.getElementById("lifecycle-" + name);
    const fields = ["side", "scenario", "status", "method", "metrics", "tracing", "propagate", "parent", "reverse", "bodySize", "captureHeaders"];
    let position = 0;
    let selection = null;
    let message = "request";
    let selectedField = null;
    let linkedNodes = [];
    function read() {
      const config = {};
      fields.forEach(name => {
        const input = byId(name);
        config[name] = input.type === "checkbox" ? input.checked : input.value;
      });
      config.status = Number(config.status);
      return config;
    }
    function node(tag, text, className) {
      const element = document.createElement(tag);
      if (text !== undefined) { element.textContent = text; }
      if (className) { element.className = className; }
      return element;
    }
    function button(text, handler, className) {
      const element = node("button", text, className);
      element.type = "button";
      element.addEventListener("click", handler);
      return element;
    }
    function details(title, child) {
      const element = node("details", undefined, "lc-disclosure");
      element.appendChild(node("summary", title));
      element.appendChild(child);
      return element;
    }
    function svg(tag, attrs, text) {
      const element = document.createElementNS("http://www.w3.org/2000/svg", tag);
      Object.entries(attrs).forEach(([key, value]) => element.setAttribute(key, String(value)));
      if (text !== undefined) { element.textContent = text; }
      return element;
    }
    function choose(type, key) {
      const collapse = type !== "http" && selection && selection.type === type && selection.key === key;
      selection = collapse ? null : { type, key };
      selectedField = null;
      render();
    }
    function move(index) {
      position = index;
      const event = events(read())[position];
      if (event.chunk) {
        message = event.chunk.direction;
        selectedField = message + "-body";
      } else if (event.phase === "headers") { message = "response"; }
      if (!event.response && message === "response") { message = "request"; }
      render();
    }
    function highlight() {
      linkedNodes.forEach(({ element, source }) => {
        element.dataset.highlighted = String(source === selectedField);
        element.setAttribute("aria-pressed", String(source === selectedField));
      });
      const notes = {
        method: "The request method supplies http.request.method. Known methods are normalized; this fixture uses a known method.",
        target: "The path and query supply URL attributes. access_token is explicitly redacted in this example; /widgets/{id} is a configured classifier result, not the literal URL. Metric labels never use the raw URL.",
        host: "The request authority supplies server.address and server.port. Server metric address/port opt-ins are off in this example.",
        status: "Response status supplies http.response.status_code at completion. Default client errors: 4xx/5xx; server: 5xx. Observed exceptions take precedence for error.type.",
        protocol: "network.protocol.version comes from response metadata on the client and request metadata on the server.",
        traceparent: "Trace Context links parent and child spans. This header is not automatically a span attribute. Request propagation and server extraction require tracing in this middleware.",
        baggage: "Baggage travels in context and supported propagation headers. It is not automatically copied to span attributes or metric labels.",
        "response-length": "Content-Length declares the representation length; it does not measure consumption. The declared strategy can record it for incomplete bodies. HEAD/204 have protocol-known zero payload size.",
        "request-length": "This Content-Length is computed from the UTF-8 request fixture. It is independent of which body chunks have been evaluated.",
        "response-body": "Chunk counts and byte totals come from the exact fixture slices below. Observe records after normal EOF and cleanup; incomplete observations are omitted. Protocol-known zero sizes do not need observation. Body contents are not span attributes.",
        "request-body": "The request preview includes only evaluated fixture chunks. Request body-size metrics are not opted into in this example. Body contents are not span attributes.",
        exception: "This observed exception is recorded as an exception event and error.type when the HTTP span ends. It is not an HTTP header."
      };
      byId("field-note").hidden = !selectedField;
      byId("field-note").textContent = selectedField ? notes[selectedField] || "Header capture is opt-in. This fixture header is allowed by the default HeaderRedactor; propagation headers are elided." : "";
    }
    function linked(text, source, className) {
      const element = button(text, () => { selectedField = selectedField === source ? null : source; highlight(); }, className || "lc-linked");
      linkedNodes.push({ element, source });
      return element;
    }
    function rows(entries) {
      const group = node("div", undefined, "lc-attributes");
      entries.forEach(entry => {
        const row = linked("", entry.source, "lc-detail-row lc-linked");
        row.appendChild(node("span", entry.key));
        const value = node("span", undefined, "lc-attribute-value");
        value.appendChild(node("code", Array.isArray(entry.value) ? JSON.stringify(entry.value) : String(entry.value)));
        if (entry.destinations && entry.destinations.length) {
          const badges = node("span", undefined, "lc-destinations");
          entry.destinations.forEach(destination => badges.appendChild(node("span", destination, "lc-destination")));
          value.appendChild(badges);
        }
        row.appendChild(value);
        group.appendChild(row);
      });
      return group;
    }
    function summaryRows(entries) {
      const group = node("div", undefined, "lc-attributes");
      entries.forEach(([key, value]) => {
        const row = node("div", undefined, "lc-detail-row");
        row.appendChild(node("span", key));
        row.appendChild(node("code", String(value)));
        group.appendChild(row);
      });
      return group;
    }
    function graph(timeline, event, metric, config) {
      const chart = svg("svg", { viewBox: "0 0 360 54", preserveAspectRatio: "none", role: "img", "aria-label": metric === "active"
        ? "Active request count, step plot" : metric === "duration" ? "Duration measurement, milliseconds" : "Response-size measurement, bytes" });
      const x = time => 20 + time / 200 * 320;
      const max = metric === "active" ? 1 : metric === "duration" ? 150 : Math.max(1, fixture(config).response.declaredLength || 1);
      const y = value => 44 - value / max * 34;
      chart.appendChild(svg("line", { x1: 20, x2: 340, y1: 44, y2: 44, class: "lc-grid-line" }));
      chart.appendChild(svg("line", { x1: x(event.time), x2: x(event.time), y1: 3, y2: 49, class: "lc-time-cursor" }));
      if (metric === "active" && config.metrics) {
        let path = "M 20 " + y(0);
        timeline.slice(0, position + 1).forEach(entry => { path += " H " + x(entry.time) + " V " + y(entry.active); });
        chart.appendChild(svg("path", { d: path, class: "lc-active-line" }));
        chart.appendChild(svg("circle", { cx: x(event.time), cy: y(event.active), r: 3, class: "lc-chart-point" }));
      } else if (metric !== "active") {
        const sample = timeline.slice(0, position + 1).find(entry => entry.phase === "metrics" && entry.duration && (metric !== "size" || entry.bodySize !== null));
        if (sample) {
          const value = metric === "duration" ? sample.durationSeconds * 1000 : sample.bodySize;
          chart.appendChild(svg("line", { x1: x(sample.time), x2: x(sample.time), y1: 44, y2: y(value), class: "lc-duration-line" }));
          chart.appendChild(svg("circle", { cx: x(sample.time), cy: y(value), r: 4, class: "lc-chart-point" }));
        }
      }
      return chart;
    }
    function traceRow(label, end, state, detail, key, parent) {
      const row = button("", () => choose("span", key), "lc-signal-row lc-trace-select");
      row.dataset.selected = String(selection && selection.type === "span" && selection.key === key);
      row.setAttribute("aria-label", "Expand " + label + ": " + detail);
      row.setAttribute("aria-expanded", row.dataset.selected);
      row.setAttribute("aria-controls", "lifecycle-inspector");
      const name = node("span", undefined, "lc-signal-label");
      name.appendChild(node("span", (row.dataset.selected === "true" ? "▾ " : "▸ ") + label));
      name.appendChild(node("small", detail));
      row.appendChild(name);
      const track = node("span", undefined, "lc-trace-track");
      const inner = node("span", undefined, "lc-trace-inner");
      const bar = node("span", undefined, "lc-trace-bar lc-span-" + state);
      bar.style.left = (parent ? 0 : 2.5) + "%";
      bar.style.width = Math.max(1, (end - (parent ? 0 : 5)) / 2) + "%";
      inner.appendChild(bar);
      track.appendChild(inner);
      row.appendChild(track);
      return row;
    }
    function renderMessage(data, event) {
      const container = node("div", undefined, "lc-message");
      const tabs = node("div", undefined, "lc-message-tabs");
      ["request", "response"].forEach(direction => {
        const tab = button(direction === "request" ? "Request" : "Response", () => { message = direction; render(); }, "lc-tab");
        tab.disabled = direction === "response" && !event.response;
        tab.setAttribute("aria-pressed", String(message === direction));
        tabs.appendChild(tab);
      });
      container.appendChild(tabs);
      const response = message === "response";
      const model = data.messages[message];
      let messageNote = !data.started ? "Prepared request · not evaluated yet" : "";
      if (data.started && !response && data.config.side === "client" && !data.requestHeader) {
        messageNote = "Context injection off: " + (!data.config.tracing ? "tracing is disabled." : "the propagation filter is disabled.");
      } else if (data.started && !response && data.config.side === "server") {
        if (!data.config.tracing && data.requestHeader) { messageNote = "Incoming context is not extracted: tracing is disabled."; }
        else if (!data.requestHeader) { messageNote = "No incoming trace context."; }
      } else if (response && data.config.side === "server" && !data.responseHeader) {
        messageNote = "Response context injection off: " + (!data.config.tracing ? "tracing is disabled." : "reverse propagation is disabled.");
      }
      if (messageNote) { container.appendChild(node("p", messageNote, "lc-muted")); }
      const raw = node("div", undefined, "lc-http-code");
      const line = node("div", undefined, "lc-http-line");
      if (response) {
        line.appendChild(linked("HTTP/" + model.protocol + " ", "protocol", "lc-http-field"));
        line.appendChild(linked(model.status + " " + model.reason, "status", "lc-http-field"));
      } else {
        line.appendChild(linked(model.method + " ", "method", "lc-http-field"));
        line.appendChild(linked(model.path + "?" + model.query + " ", "target", "lc-http-field"));
        line.appendChild(linked("HTTP/" + model.protocol, "protocol", "lc-http-field"));
      }
      raw.appendChild(line);
      model.headers.forEach(([key, value]) => {
        const header = node("div", undefined, "lc-http-line");
        header.appendChild(linked(key + ": " + value, headerSource(message, key), "lc-http-field"));
        raw.appendChild(header);
      });
      const seen = progress(model.body, response ? event.chunkCount : event.requestChunkCount);
      // Form-control boxes disrupt line flow when a chunk contains newlines or completes UTF-8.
      // Keep decoded chunk text in inline spans; the whole preview is one accessible control.
      const preview = node("pre", undefined, "lc-body-preview");
      const bodySource = message + "-body";
      const selectBody = () => { selectedField = selectedField === bodySource ? null : bodySource; highlight(); };
      preview.setAttribute("role", "button");
      preview.setAttribute("tabindex", "0");
      preview.setAttribute("aria-label", "Evaluated " + message + " body, " + seen.bytes.length + " bytes. Select to inspect body measurement.");
      preview.addEventListener("click", selectBody);
      preview.addEventListener("keydown", event => {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          selectBody();
        }
      });
      linkedNodes.push({ element: preview, source: bodySource });
      seen.segments.forEach((text, index) => {
        const segment = node("span", text, "lc-body-chunk");
        segment.dataset.latest = String(!!event.chunk && event.chunk.direction === message && event.chunk.index === index);
        segment.setAttribute("title", "Chunk " + (index + 1) + " · bytes [" + seen.chunks[index].start + ", " + seen.chunks[index].end + ")");
        preview.appendChild(segment);
      });
      if (seen.text) { raw.appendChild(preview); }
      container.appendChild(raw);
      const complete = response ? event.bodyComplete : event.requestComplete;
      if (seen.total) {
        const state = complete ? "EOF observed" : seen.bytes.length ? "EOF not observed" : "unread";
        container.appendChild(linked(seen.bytes.length + "/" + seen.total + " B evaluated · " + state,
          message + "-body", "lc-body-progress lc-linked"));
      } else {
        const state = complete ? "" : response && event.bodyStarted ? " · EOF not observed" : " · not evaluated";
        container.appendChild(node("p", "Empty body" + state + ".", "lc-muted"));
      }
      if (seen.pendingBytes) {
        container.appendChild(node("p", seen.pendingBytes + " UTF-8 byte is waiting for the next chunk to complete its character.", "lc-muted"));
      }
      if (event.chunk && event.chunk.direction === message) {
        const chunk = event.chunk;
        const debug = summaryRows([["Byte range", "[" + chunk.start + ", " + chunk.end + ") · " + chunk.bytes.length + " B"],
          ["UTF-8 bytes (hex)", chunk.bytes.map(byte => byte.toString(16).padStart(2, "0")).join(" ")]]);
        container.appendChild(details("Chunk " + (chunk.index + 1) + "/" + model.body.chunks.length + " · exact bytes", debug));
      }
      const unread = remainder(model.body, response ? event.chunkCount : event.requestChunkCount);
      if (unread.bytes.length) {
        const example = node("div");
        example.appendChild(node("p", "Example data awaiting evaluation.", "lc-muted"));
        if (unread.completesCharacter) {
          example.appendChild(node("p", "The first character completes the UTF-8 sequence begun in the evaluated bytes.", "lc-muted"));
        }
        example.appendChild(node("pre", unread.text, "lc-fixture-preview"));
        container.appendChild(details("Remaining example body · " + unread.bytes.length + " B", example));
      }
      return container;
    }
    function renderInspector(data, event) {
      byId("inspector").hidden = !selection;
      if (!selection) { return; }
      linkedNodes = [];
      const content = byId("inspector-content");
      content.replaceChildren();
      const prefix = "http." + data.config.side;
      let entries = [];
      let title;
      let description;
      if (selection.type === "span") {
        const parent = selection.key === "parent";
        title = parent ? "External parent context" : data.config.method + " " + data.messages.request.template;
        description = parent ? "Only the parent context is known. Its attributes, events and lifetime are unavailable."
          : !data.traced ? "No HTTP span exists at this step." : "";
        if (data.traced && !parent) { entries = data.spanAttributes; }
        const identityValues = parent
          ? [["Trace ID", data.started && data.config.parent ? ids.trace : "unknown"],
            ["Span ID", data.config.side === "client" ? ids.parent : ids.client], ["Context baggage", data.config.parent ? "tenant=demo" : "none"]]
          : [["Trace ID", data.traced ? data.traceId : "—"], ["Span ID", data.traced ? data.local : "—"],
            ["Parent", data.traced ? data.parentId || "root" : "—"]];
        if (data.traced && !parent) {
          const ended = event.span === "ended";
          const elapsed = Math.round(event.completed ? event.durationSeconds * 1000 : event.time - 5);
          content.appendChild(node("p", (data.config.side === "client" ? "Client" : "Server") + " · Status: " + data.spanStatus +
            " · " + (ended ? "Ended" : "Open") + " · " + elapsed + (ended ? " ms" : " ms elapsed"), "lc-span-summary"));
        }
        const identity = node("dl", undefined, "lc-span-identity");
        identityValues.forEach(([label, value]) => {
          const field = node("div");
          field.appendChild(node("dt", label));
          const definition = node("dd");
          definition.appendChild(node("code", value));
          field.appendChild(definition);
          identity.appendChild(field);
        });
        content.appendChild(identity);
        if (data.traced && !parent) {
          content.appendChild(details("Timing", summaryRows([["Start", "5 ms"],
            ["End", event.span === "ended" ? Math.round(event.durationSeconds * 1000 + 5) + " ms" : "not ended"]])));
        }
        if (!parent) {
          const context = rows([
            { key: "Source span context", value: data.started ? data.scope || "none" : "not running", source: "traceparent" },
            { key: "Captured metric context", value: data.started && data.config.metrics ? data.scope || "none" : "not recording", source: "traceparent" },
            { key: "Body consumer", value: data.config.side === "client" && data.config.parent ? "caller " + ids.parent : "backend context, unchanged", source: "traceparent" },
            { key: "Source baggage", value: data.started ? data.baggage : "not running", source: "baggage" }
          ]);
          content.appendChild(details("Execution context & baggage", context));
          const exception = event.span === "ended" && event.errorType && event.errorType.startsWith("demo.");
          if (exception) {
            content.appendChild(details("Span events · 1 exception", rows([
              { key: "exception.type", value: event.errorType, source: "exception" },
              { key: "exception.message", value: "Simulated " + (event.errorType === "demo.BodyFailure" ? "body source failure" : "acquisition/app failure"), source: "exception" }
            ])));
          }
        }
      } else if (selection.type === "metric") {
        const metric = selection.key;
        const active = metric === "active";
        const size = metric === "size";
        const recorded = data.config.metrics && (active ? data.started : size ? data.sizeCount > 0 : data.count > 0);
        title = prefix + (active ? ".active_requests" : size ? ".response.body.size" : ".request.duration");
        description = active ? "Up/down counter · {request}. The same attributes balance increments and decrements."
          : "Histogram · " + (size ? "By" : "s") + ". " + (size && recorded
            ? data.messages.response.noBody ? "Protocol-known zero payload." : data.config.bodySize === "observe" ? "Recorded from a complete observation." : "Recorded from declared length."
            : measurementState(data, event, metric) + ".");
        if (active && !data.config.metrics) { description = "Up/down counter · {request}. Disabled by metrics filter."; }
        if (data.config.metrics && data.started && (active || recorded)) { entries = active ? data.startAttributes : data.terminalAttributes; }
        if (!active && !size && recorded) { content.appendChild(summaryRows([["Sample", event.durationSeconds.toFixed(3) + " s"]])); }
        if (!active) {
          const buckets = size ? data.sizeBuckets : data.buckets;
          content.appendChild(details("Histogram buckets · cumulative counts", summaryRows(buckets.map(bucket => ["le=" + (bucket.le === Infinity ? "+Inf" : bucket.le), bucket.count]))));
          if (recorded && data.exemplarSpan) {
            content.appendChild(button("Example exemplar → span " + data.exemplarSpan, () => {
              choose("span", data.exemplarSpan === data.local ? "local" : "parent");
            }, "lc-exemplar"));
          }
        }

      } else {
        title = "HTTP messages & related telemetry";
        description = "";
        const destinations = (attributes, names) => attributes.map(entry => Object.assign({}, entry, { destinations: names }));
        entries = destinations(data.spanAttributes, ["Span"]);
        if (data.started && data.config.metrics) { entries.push(...destinations(data.startAttributes, ["Active requests"])); }
        if (event.duration) {
          entries.push(...destinations(data.terminalAttributes, event.bodySize !== null ? ["Duration", "Body size"] : ["Duration"]));
        }
        if (event.bodySize !== null) {
          entries.push({ key: prefix + ".response.body.size", value: event.bodySize + " B",
            source: data.messages.response.noBody ? "response-body" : data.config.bodySize === "contentLength" ? "response-length" : "response-body",
            destinations: ["Measurement"] });
        }
        if (data.started) {
          entries.push({ key: "HTTP source span", value: data.scope || "none", source: "traceparent", destinations: ["Context"] },
            { key: "HTTP source baggage", value: data.baggage, source: "baggage", destinations: ["Context"] });
        }
        entries = groupAttributes(entries);
      }
      byId("inspector-title").textContent = title;
      if (description) { content.appendChild(node("p", description, "lc-muted")); }
      content.appendChild(node("hr", undefined, "lc-message-separator"));
      const grid = node("div", undefined, "lc-inspector-grid");
      grid.appendChild(renderMessage(data, event));
      const attributes = node("div", undefined, "lc-related");
      attributes.appendChild(node("h4", selection.type === "http" ? "Related telemetry at this step" : "Attributes at this step"));
      attributes.appendChild(entries.length ? rows(entries) : node("p", "No attributes available for this selection at this step.", "lc-muted"));
      grid.appendChild(attributes);
      content.appendChild(grid);
      highlight();
    }
    function render() {
      // Park the inspector before replacing rows, preserving its DOM nodes and unique IDs.
      const inspector = byId("inspector");
      byId("inspector-home").appendChild(inspector);
      const config = read();
      const timeline = events(config);
      position = Math.max(0, Math.min(position, timeline.length - 1));
      const event = timeline[position];
      const data = snapshot(config, event);
      byId("parent-label").textContent = config.side === "client" ? "Caller context + baggage" : "Incoming context + baggage";
      byId("propagate").disabled = config.side !== "client";
      byId("reverse").disabled = config.side !== "server";
      byId("steps").replaceChildren();
      timeline.forEach((entry, index) => {
        const item = button(entry.time + " ms · " + entry.label, () => move(index), "lc-event");
        item.dataset.reached = String(index <= position);
        if (index === position) { item.setAttribute("aria-current", "step"); }
        byId("steps").appendChild(item);
      });
      byId("next").disabled = position === timeline.length - 1;
      byId("back").disabled = position === 0;
      byId("state").textContent = event.time + " ms · " + event.label;
      byId("explanation").textContent = event.explanation;
      byId("metrics-overview").replaceChildren();
      const summaries = [
        ["active", "Active requests", !config.metrics ? "disabled" : event.active + " requests"],
        ["duration", "Duration", !config.metrics ? "disabled" : data.count ? Math.round(data.sum * 1000) + " ms · 1 observation" : measurementState(data, event, "duration")],
        ["size", "Response body size", !config.metrics ? "disabled" : event.bodySize === null ? measurementState(data, event, "size")
          : event.bodySize + " B · " + (data.messages.response.noBody ? "protocol-known zero" : config.bodySize === "observe" ? "observed" : "declared")]
      ];
      summaries.forEach(([key, label, value]) => {
        const row = node("div", undefined, "lc-signal-row lc-metric-row");
        row.dataset.selected = String(selection && selection.type === "metric" && selection.key === key);
        const summary = button("", () => choose("metric", key), "lc-metric-summary");
        summary.setAttribute("aria-expanded", row.dataset.selected);
        summary.setAttribute("aria-controls", "lifecycle-inspector");
        summary.appendChild(node("span", (row.dataset.selected === "true" ? "▾ " : "▸ ") + label));
        summary.appendChild(node("strong", value));
        const plot = button("", () => {
          if (key !== "active" && event.duration && (key !== "size" || event.bodySize !== null)) {
            position = timeline.findIndex(entry => entry.phase === "metrics");
          }
          choose("metric", key);
        }, "lc-mini-plot");
        plot.setAttribute("aria-label", "Expand " + label + " measurement and attributes");
        plot.setAttribute("aria-expanded", row.dataset.selected);
        plot.setAttribute("aria-controls", "lifecycle-inspector");
        plot.appendChild(graph(timeline, event, key, config));
        row.appendChild(summary);
        row.appendChild(plot);
        byId("metrics-overview").appendChild(row);
        if (row.dataset.selected === "true") { byId("metrics-overview").appendChild(inspector); }
      });
      const axis = svg("svg", { viewBox: "0 0 360 22", role: "img", "aria-label": "Shared simulated time axis, 0 to 200 milliseconds" });
      [0, 50, 100, 150, 200].forEach(time => axis.appendChild(svg("text", { x: 20 + time / 200 * 320, y: 15, "text-anchor": "middle" }, time + " ms")));
      byId("time-axis").replaceChildren(axis);
      byId("trace").replaceChildren();
      if (data.started && config.parent) {
        byId("trace").appendChild(traceRow(config.side === "client" ? "Caller span" : "Remote parent", event.time, "parent", "external lifetime", "parent", true));
        if (selection && selection.type === "span" && selection.key === "parent") { byId("trace").appendChild(inspector); }
      }
      if (data.traced) {
        const terminal = timeline.find(entry => entry.phase === "metrics");
        const end = event.completed && terminal ? terminal.time : event.time;
        const state = event.span === "ended" ? event.error ? "error" : "ended" : "open";
        byId("trace").appendChild(traceRow(config.method + " " + data.messages.request.template, end, state,
          (state === "error" ? "Error" : event.span) + " · " + (end - 5) + " ms", "local", false));
        if (selection && selection.type === "span" && selection.key === "local") { byId("trace").appendChild(inspector); }
      } else {
        byId("trace").appendChild(node("p", config.tracing ? "HTTP span has not started." : "Tracing is off. No HTTP span is created.", "lc-muted"));
      }
      byId("response").disabled = !event.response;
      renderInspector(data, event);
      root.hidden = false;
    }
    fields.forEach(name => byId(name).addEventListener("change", () => {
      position = 0; selection = null; message = "request"; selectedField = null; render();
    }));
    byId("request").addEventListener("click", () => { message = "request"; choose("http", "request"); });
    byId("response").addEventListener("click", () => { message = "response"; choose("http", "response"); });
    byId("inspector-close").addEventListener("click", () => { selection = null; render(); });
    byId("reset").addEventListener("click", () => { position = 0; selection = null; message = "request"; selectedField = null; render(); });
    byId("next").addEventListener("click", () => move(position + 1));
    byId("back").addEventListener("click", () => move(position - 1));
    render();
  }
  if (document.readyState === "loading") { document.addEventListener("DOMContentLoaded", mount); }
  else { mount(); }
}());
