import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "0.3" // your current series x.y

ThisBuild / organization := "io.chrisdavenport"
ThisBuild / organizationName := "Christopher Davenport"
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("christopherdavenport", "Christopher Davenport")
)

ThisBuild / tlCiReleaseBranches := Seq("main")

// true by default, set to false to publish to s01.oss.sonatype.org
ThisBuild / tlSonatypeUseLegacyHost := true

val scala212 = "2.12.18"
val scala213 = "2.13.12"
val scala3 = "3.3.1"
ThisBuild / crossScalaVersions := Seq(scala212, scala213, scala3)
ThisBuild / scalaVersion := scala3

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

val catsV = "2.10.0"
val catsEffectV = "3.5.2"
val catsMtlV = "1.3.1"
val fs2V = "3.9.2"
val http4sV = "0.23.23"

val openTelemetryV = "1.31.0"
val otel4sV = "0.3.0-RC2"

val munitCatsEffectV = "2.0.0-M3"

val slf4jV    = "1.7.30"


// Projects
lazy val `natchez-http4s-otel` = tlCrossRootProject
  .aggregate(core, examples)

lazy val core = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "natchez-http4s-otel",
    mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[DirectMissingMethodProblem]("io.chrisdavenport.natchezhttp4sotel.ClientMiddleware.request"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("io.chrisdavenport.natchezhttp4sotel.ServerMiddleware.request")
    ),

    libraryDependencies ++= Seq(
      "org.typelevel"               %%% "cats-core"                  % catsV,
      "org.typelevel"               %%% "cats-effect"                % catsEffectV,

      "co.fs2"                      %%% "fs2-core"                   % fs2V,
      "co.fs2"                      %%% "fs2-io"                     % fs2V,

      "org.http4s"                  %%% "http4s-server"        % http4sV,
      "org.http4s"                  %%% "http4s-client"        % http4sV,

      "org.typelevel"               %%% "otel4s-core-trace" % otel4sV,
      "org.typelevel"               %%% "otel4s-java"       % otel4sV,
      "org.typelevel"               %%% "cats-mtl" % catsMtlV,


      "org.typelevel"               %%% "munit-cats-effect"        % munitCatsEffectV         % Test,
    )
  )

lazy val examples = project.in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core.jvm)
  .settings(
    scalacOptions        -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel"    %% "otel4s-java" % otel4sV,
      "io.opentelemetry" % "opentelemetry-exporter-otlp" % openTelemetryV % Runtime,
      "io.opentelemetry" % "opentelemetry-sdk-extension-autoconfigure" % openTelemetryV % Runtime,
      "org.http4s"   %% "http4s-dsl"          % http4sV,
      "org.http4s"   %% "http4s-ember-server" % http4sV,
      "org.http4s"   %% "http4s-ember-client" % http4sV,
      "org.slf4j"     % "slf4j-simple"        % slf4jV,
    ),
    run / fork := true,
    javaOptions += "-Dotel.service.name=jaeger-example",
    javaOptions += "-Dotel.metrics.exporter=none",
    javaOptions += "-Dotel.java.global-autoconfigure.enabled=true",
  )

lazy val site = project.in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
