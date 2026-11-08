# Streaming lifecycle

An http4s app can return a response before its body runs. The middleware needs to account for
both steps. Use the [Lifecycle explorer](lifecycle.md) to watch them one event at a time.

## When recording finishes

The combined middleware and metrics-only middleware use the rules below.

| Scenario | Client | Server |
| --- | --- | --- |
| Response acquisition or server app fails | Finishes with an error | Finishes with an error |
| Response acquisition or server app is canceled | Finishes | Finishes |
| Body finishes | Finishes after body cleanup | Finishes after body cleanup |
| Body fails | Finishes with the exception | Finishes with the exception |
| Started body is canceled | Finishes after body cleanup | Finishes after body cleanup |
| Body is never started | Finishes when the response resource is released | Keeps waiting for the body |

Cancellation alone does not count as an error. A response status that already counts as an error
still does.

When recording finishes, the middleware records duration and any available body sizes, then
reduces the active-request count if enabled. The combined middleware then ends the HTTP span.
This happens once per request.

The tracing-only builders use different rules. Client spans end when the response resource is
released. Server spans end when the app returns a response.

## Server bodies must run

A server `HttpApp` returns a response, but has no response resource to release later. If nobody
runs the body, the middleware has no signal that the request is finished. The span stays open
and the active-request count stays incremented. Even empty and HEAD response bodies must run,
although their response size is known to be zero.

Wrap the **final app** with instrumentation. Put routing, error handlers and middleware that
changes the response inside it. An outer middleware that replaces a response without running its
original body can leave that request's telemetry unfinished.

Recording includes response-body cleanup. It does not prove that the remote client received the
bytes.

## Always release client responses

Release the response resource even when you do not read the body. The middleware finishes
recording when an unread response is released. An observed body size is omitted if the body was
not fully read, but a declared Content-Length can still be recorded.

Recording finishes before the client's backend resource cleanup. Errors in that later cleanup or
in caller code do not count as HTTP failures.

## Body context and errors

Request and response body effects keep the HTTP context captured for the request. The caller or
backend code that consumes the stream keeps its own context.

Exceptions seen by the middleware take precedence over errors based on the response status.
Client and server errors can differ for the same request. For example, a server body failure may
appear to the client as an error, a truncated body or normal completion, depending on the
backend. Each side records the outcome it sees.

## Check your application

Try a streamed response, a body failure, cancellation, an unread client response and an empty
server response with your backend and middleware stack. If server spans stay open or active
requests keep increasing, check whether middleware outside the instrumentation discards bodies.
