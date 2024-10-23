# http4s-otel4s-middleware

## Quick Start

To use http4s-otel4s-middleware in an existing SBT project with Scala 2.13 or a
later version, add one or more of the following dependencies to your `build.sbt`
depending on your needs:

```scala
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-otel4s-middleware-core" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-metrics" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-trace-core" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-trace-client" % "<version>",
  "org.http4s" %% "http4s-otel4s-middleware-trace-server" % "<version>",
)
```
