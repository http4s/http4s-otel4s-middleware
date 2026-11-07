# Getting started

Examples use Scala 3. The library also supports Scala 2.13.

## Dependencies

For metrics and tracing on both sides, add:

```scala
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-otel4s-middleware-client" % "@VERSION@",
  "org.http4s" %% "http4s-otel4s-middleware-server" % "@VERSION@"
)
```

Install only the side you need. These modules include the tracing and metrics modules transitively;
you do not need to list `core` separately. For metrics alone, use `metrics-client` or
`metrics-server`; for tracing alone, use `trace-client` or `trace-server`.

In an sbt cross-project, use `%%%` instead of `%%`. Choose an otel4s backend that supports your
platform. The middleware does not configure exporters or start a collector.

## A complete JVM client

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

      val client = for {
        raw <- EmberClientBuilder.default[IO].build
        instrumented <- Resource.eval(
          ClientInstrumentation.middleware[IO](clientConfig, tracingConfig).map(_(raw))
        )
      } yield instrumented

      client.use(_.expect[String](uri"http://localhost:8080/hello").flatMap(IO.println))
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
instance. Keep its resource alive for the lifetime of the instrumented application.
`expect` consumes and releases the response; with `client.run`, always use the returned resource.

Continue with [Client instrumentation](client.md) or [Server instrumentation](server.md).
