/* Illustrative states for the combined middleware; no network requests are made. */
(function () {
  "use strict";

  // Scenario names follow ClientInstrumentationLifecycleTest and ServerInstrumentationLifecycleTest.
  function events(side, scenario) {
    const client = side === "client";
    const state = (label, span, active, duration, error, explanation) =>
      ({ label, span, active, duration, error, explanation });
    const result = [
      state("Before request", "not started", 0, false, false, "No exchange has started."),
      state(client ? "Acquire response resource" : "Run app", "open", 1, false, false,
        "Start the HTTP span and increment active requests.")
    ];
    const finish = (label, error, explanation) =>
      state(label, "ended", 0, true, error, explanation);
    if (scenario === "app-error" || scenario === "app-cancel") {
      const error = scenario === "app-error";
      result.push(finish(error ? "Acquisition / app fails" : "Acquisition / app canceled", error,
        error ? "Record the observed exception, record metrics, then end the span."
          : "Record metrics and end the span. Cancellation alone is neutral."));
      return result;
    }
    result.push(state("Response headers available (200)", "open", 1, false, false,
      "Creating a response does not complete the exchange."));
    if (scenario === "unread") {
      result.push(client
        ? finish("Release unread response resource", false,
          "Release completes the client exchange before backend release finalizers. Observed body size is unavailable.")
        : state("Body discarded without starting", "open", 1, false, false,
          "The server has no completion signal. The span stays open, active requests stay incremented, and duration is missing."));
      return result;
    }
    result.push(state("Evaluate response body", "open", 1, false, false,
      "Body source effects run in the HTTP context. The consumer retains its own context."));
    if (scenario === "body-error") {
      result.push(finish("Body fails and cleanup finishes", true,
        "The observed body exception takes precedence over status classification. Record metrics, then end the span."));
    } else if (scenario === "body-cancel") {
      result.push(finish("Body canceled and cleanup finishes", false,
        "Complete the exchange. Cancellation alone does not mark this 200 response as an error."));
    } else {
      result.push(finish("Body completes, including cleanup", false,
        "Record duration and available sizes, decrement active requests, then end the span. This does not prove network delivery."));
    }
    if (client) {
      result.push(finish("Release response resource", scenario === "body-error",
        "Completion has already been recorded. Backend release does not record a second duration or end the span again."));
    }
    return result;
  }

  // Expose the pure model for checking all scenarios independently of the UI.
  if (typeof module !== "undefined" && module.exports) module.exports = { events };
  if (typeof document === "undefined") return;
  function mount() {
    const root = document.getElementById("lifecycle-explorer");
    if (!root) return;
    const side = document.getElementById("lifecycle-side");
    const scenario = document.getElementById("lifecycle-scenario");
    const steps = document.getElementById("lifecycle-steps");
    const next = document.getElementById("lifecycle-next");
    let position = 0;
    function render() {
      const timeline = events(side.value, scenario.value);
      steps.replaceChildren();
      timeline.forEach((event, index) => {
        const item = document.createElement("li");
        item.textContent = event.label;
        item.dataset.reached = String(index <= position);
        if (index === position) item.setAttribute("aria-current", "step");
        steps.appendChild(item);
      });
      const current = timeline[position];
      document.getElementById("lifecycle-state").textContent =
        "Span: " + current.span + " · Active requests: " + current.active +
        " · Duration: " + (current.duration ? "recorded" : "not recorded") +
        " · Error: " + (current.error ? "yes" : "no");
      document.getElementById("lifecycle-explanation").textContent = current.explanation;
      next.disabled = position === timeline.length - 1;
    }
    function reset() { position = 0; render(); }
    side.addEventListener("change", reset);
    scenario.addEventListener("change", reset);
    document.getElementById("lifecycle-reset").addEventListener("click", reset);
    next.addEventListener("click", function () { position += 1; render(); });
    root.hidden = false;
    render();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", mount);
  else mount();
}());
