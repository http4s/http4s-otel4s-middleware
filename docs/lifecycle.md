# Lifecycle and limitations

HTTP response creation and body completion are separate events in http4s. The rules below apply
to **combined instrumentation and standalone metrics**. Existing tracing-only builders use
response resource scope on the client and app evaluation on the server.

## Completion boundaries

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

## Lifecycle explorer

This simulation illustrates the rules above with tracing and active requests enabled. It does
not issue requests. Scenarios correspond to the client and server instrumentation lifecycle test
suites; timing is illustrative. All scenarios use a 200 response, so only observed exceptions set
error status.

<div id="lifecycle-explorer" class="lifecycle-explorer" hidden>
  <div class="lifecycle-controls">
    <label>Side <select id="lifecycle-side"><option value="client">Client</option><option value="server">Server</option></select></label>
    <label>Scenario <select id="lifecycle-scenario"><option value="success">Body completes</option><option value="body-error">Body fails</option><option value="body-cancel">Body canceled</option><option value="unread">Body never starts</option><option value="app-error">Acquisition / app fails</option><option value="app-cancel">Acquisition / app canceled</option></select></label>
  </div>
  <ol id="lifecycle-steps" class="lifecycle-steps"></ol>
  <div class="lifecycle-controls"><button type="button" id="lifecycle-reset">Reset</button><button type="button" id="lifecycle-next">Next event</button></div>
  <div id="lifecycle-state" role="status" aria-live="polite" aria-atomic="true"></div>
  <p id="lifecycle-explanation"></p>
</div>

With JavaScript disabled, the completion table above provides the same rules.

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
