import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "0.12" // your current series x.y

ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers += tlGitHubDev("rossabaker", "Ross A. Baker")
ThisBuild / startYear := Some(2023)

ThisBuild / tlCiReleaseBranches := Seq("main")

val scala213 = "2.13.16"
val scala3 = "3.3.5"
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / scalaVersion := scala213

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

val catsEffectV = "3.6.0"
val http4sV = "0.23.30"
val munitV = "1.0.0"
val munitCatsEffectV = "2.0.0"
val openTelemetryV = "1.48.0"
val otel4sV = "0.12.0"
val slf4jV = "1.7.36"

val baseName = "http4s-otel4s-middleware"

val sharedSettings = Seq(
  libraryDependencies ++= Seq(
    "org.http4s" %%% "http4s-core" % http4sV,
    "org.typelevel" %%% "otel4s-core-common" % otel4sV,
    "org.typelevel" %%% "otel4s-semconv" % otel4sV,
    "org.typelevel" %%% "cats-effect-testkit" % catsEffectV % Test,
    "org.scalameta" %%% "munit" % munitV % Test,
    "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test,
    "org.typelevel" %%% "otel4s-sdk-testkit" % otel4sV % Test,
  )
)

// Projects
lazy val `http4s-otel4s-middleware` = tlCrossRootProject
  .aggregate(
    core,
    `core-client`,
    `core-server`,
    metrics,
    `trace-core`,
    `trace-client`,
    `trace-server`,
    examples,
  )

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core/common"))
  .enablePlugins(BuildInfoPlugin)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-core",
    buildInfoKeys := Seq(version),
    buildInfoPackage := "org.http4s.otel4s.middleware",
    buildInfoOptions += BuildInfoOption.PackagePrivate,
  )

lazy val `core-client` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core/client"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-core-client",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "otel4s-semconv-experimental" % otel4sV,
      "org.http4s" %%% "http4s-dsl" % http4sV % Test,
    ),
  )

lazy val `core-server` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core/server"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-core-server",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-dsl" % http4sV % Test
    ),
  )

lazy val metrics = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("metrics"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-metrics",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "otel4s-core-metrics" % otel4sV,
      "org.http4s" %%% "http4s-server" % http4sV % Test,
    ),
  )

lazy val `trace-core` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("trace/core"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-trace-core"
  )

lazy val `trace-client` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("trace/client"))
  .dependsOn(`trace-core`, `core-client`)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-trace-client",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "org.http4s" %%% "http4s-client" % http4sV,
      "org.typelevel" %%% "otel4s-core-trace" % otel4sV,
    ),
  )

lazy val `trace-server` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("trace/server"))
  .dependsOn(`trace-core`, `core-server`)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-trace-server",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "org.typelevel" %%% "otel4s-core-trace" % otel4sV,
    ),
  )

lazy val examples = project
  .in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(
    metrics.jvm,
    `trace-client`.jvm,
    `trace-server`.jvm,
  )
  .settings(
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "org.http4s" %%% "http4s-core" % http4sV,
      "org.http4s" %% "http4s-dsl" % http4sV,
      "org.http4s" %% "http4s-ember-server" % http4sV,
      "org.http4s" %% "http4s-ember-client" % http4sV,
      "org.typelevel" %%% "otel4s-core-common" % otel4sV,
      "org.typelevel" %% "otel4s-oteljava" % otel4sV,
      "io.opentelemetry" % "opentelemetry-exporter-otlp" % openTelemetryV % Runtime,
      "io.opentelemetry" % "opentelemetry-sdk-extension-autoconfigure" % openTelemetryV % Runtime,
      "org.slf4j" % "slf4j-simple" % slf4jV % Runtime,
    ),
    run / fork := true,
    javaOptions += "-Dotel.service.name=jaeger-example",
    javaOptions += "-Dotel.metrics.exporter=none",
    javaOptions += "-Dotel.java.global-autoconfigure.enabled=true",
  )

lazy val site = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
  .settings(tlSite := {}) // disabled until a site is set up
