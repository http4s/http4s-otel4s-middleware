# Getting started

Examples use Scala 3. The library also supports Scala 2.13.

## Dependencies

To record metrics and traces for both clients and servers, add:

```scala
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-otel4s-middleware-client" % "@VERSION@",
  "org.http4s" %% "http4s-otel4s-middleware-server" % "@VERSION@"
)
```

Add only the client or server module if you do not need both. They include the required metrics,
tracing and core dependencies.

For metrics alone, use `metrics-client` or `metrics-server`. For tracing alone, use
`trace-client` or `trace-server`.

In an sbt cross-project, use `%%%` instead of `%%`. Choose an otel4s backend that supports your
platform. Configure exporters and a collector separately.

## Send a request on the JVM

This example uses the OtelJava backend and Ember. Add these alongside the client middleware:

```scala
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-client" % "@HTTP4S_VERSION@",
  "org.typelevel" %% "otel4s-oteljava" % "@OTEL4S_VERSION@",
  "io.opentelemetry" % "opentelemetry-exporter-otlp" % "@OTELJAVA_VERSION@" % Runtime,
  "io.opentelemetry" % "opentelemetry-sdk-extension-autoconfigure" % "@OTELJAVA_VERSION@" % Runtime
)
```

```scala mdoc:compile-only
import cats.effect.{IO, IOApp, Resource}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits.*
import org.http4s.otel4s.middleware.client.{ClientInstrumentation, ClientTracingConfig}
import org.http4s.otel4s.middleware.metrics.client.ClientMetricsConfig
import org.http4s.otel4s.middleware.trace.client.{ClientSpanDataProvider, UriRedactor}
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.trace.TracerProvider

object FirstRequest extends IOApp.Simple {
  def run: IO[Unit] = {
    OtelJava.autoConfigured[IO]().use { otel =>
      given MeterProvider[IO] = otel.meterProvider
      given TracerProvider[IO] = otel.tracerProvider
      given ContextManager[IO] = otel.contextManager

      val redactor = new UriRedactor.OnlyRedactUserInfo with QueryRedactor.RedactSensitive {}
      val spanDataProvider = ClientSpanDataProvider.openTelemetry(redactor)
      val clientConfig = ClientMetricsConfig.default
      val tracingConfig = ClientTracingConfig(spanDataProvider)

      val instrumentedClient = for {
        client <- EmberClientBuilder.default[IO].build
        middleware <- Resource.eval(
          ClientInstrumentation.middleware[IO](clientConfig, tracingConfig)
        )
      } yield middleware(client)

      instrumentedClient.use { client =>
        client.expect[String](uri"http://localhost:8080/hello").flatMap(IO.println)
      }
    }
  }
}
```

Run a service at `localhost:8080/hello` and an OTLP collector before starting the example. For a
collector accepting OTLP/HTTP on port 4318, configure the application environment:

```shell
export OTEL_SERVICE_NAME=hello-client
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
export OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf
export OTEL_TRACES_EXPORTER=otlp
export OTEL_METRICS_EXPORTER=otlp
export OTEL_LOGS_EXPORTER=none
```

If using `sbt run`, fork the application JVM and add
`-Dotel.java.global-autoconfigure.enabled=true` to its Java options, as in the repository's
`examples` project. See the [otel4s documentation](https://typelevel.org/otel4s/) for backend
setup and exporter configuration.

The meter provider, tracer provider and `ContextManager` must come from the same telemetry
instance. Keep that resource open while your application uses the middleware.

`expect` reads the response body and releases the response. If you use `client.run` instead,
call `use` on the returned resource so the response is released.

Continue with [Client instrumentation](client.md) or [Server instrumentation](server.md).
