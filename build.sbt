import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "0.2" // your current series x.y

ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers += tlGitHubDev("rossabaker", "Ross A. Baker")
ThisBuild / startYear := Some(2023)

ThisBuild / tlCiReleaseBranches := Seq("main")

val scala213 = "2.13.12"
val scala3 = "3.3.1"
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / scalaVersion := scala213

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

val catsV = "2.10.0"
val catsEffectV = "3.5.2"
val http4sV = "0.23.24"

val openTelemetryV = "1.33.0"
val otel4sV = "0.4.0"

val munitCatsEffectV = "2.0.0-M4"

val slf4jV = "1.7.36"

// Projects
lazy val `http4s-otel4s-middleware` = tlCrossRootProject
  .aggregate(core, examples)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "http4s-otel4s-middleware",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV,
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "org.http4s" %%% "http4s-client" % http4sV,
      "org.typelevel" %%% "otel4s-core-trace" % otel4sV,
      "org.typelevel" %%% "otel4s-semconv" % otel4sV,
    ),
  )

lazy val `core-jvm-tests` = project
  .in(file("core-jvm-tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core.jvm)
  .settings(
    libraryDependencies ++= Seq(
      "io.opentelemetry" % "opentelemetry-sdk-testing" % openTelemetryV % Test,
      "org.typelevel" %%% "cats-effect-testkit" % catsEffectV % Test,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test,
      "org.typelevel" %%% "otel4s-java" % otel4sV % Test,
    )
  )

lazy val examples = project
  .in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core.jvm)
  .settings(
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "otel4s-java" % otel4sV,
      "io.opentelemetry" % "opentelemetry-exporter-otlp" % openTelemetryV % Runtime,
      "io.opentelemetry" % "opentelemetry-sdk-extension-autoconfigure" % openTelemetryV % Runtime,
      "org.http4s" %% "http4s-dsl" % http4sV,
      "org.http4s" %% "http4s-ember-server" % http4sV,
      "org.http4s" %% "http4s-ember-client" % http4sV,
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
