import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "0.19" // your current series x.y

ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers += tlGitHubDev("rossabaker", "Ross A. Baker")
ThisBuild / startYear := Some(2023)

ThisBuild / tlCiReleaseBranches := Seq("main")

val scala213 = "2.13.18"
val scala3 = "3.3.9"
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / scalaVersion := scala213

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

val catsEffectV = "3.7.1"
val http4sV = "0.23.38"
val munitV = "1.3.6"
val munitCatsEffectV = "2.2.1"
val openTelemetryV = "1.67.0"
val otel4sV = "1.1.0"
val otel4sSdkV = "0.19.4"
val slf4jV = "1.7.36"

val baseName = "http4s-otel4s-middleware"

// Tests run on Node.js and use fs2 APIs that import Node built-in modules.
val sharedJsTestSettings = Seq(
  Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

val sharedTestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "otel4s-semconv-experimental" % otel4sV % Test,
    "org.typelevel" %%% "cats-effect-testkit" % catsEffectV % Test,
    "org.scalameta" %%% "munit" % munitV % Test,
    "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test,
    "org.typelevel" %%% "otel4s-sdk-testkit" % otel4sSdkV % Test,
  )
)

val sharedSettings = sharedTestSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.http4s" %%% "http4s-core" % http4sV,
    "org.typelevel" %%% "otel4s-core-common" % otel4sV,
    "org.typelevel" %%% "otel4s-semconv" % otel4sV,
  )
)

// Projects
lazy val `http4s-otel4s-middleware` = tlCrossRootProject
  .aggregate(
    core,
    `core-client`,
    `core-server`,
    metrics,
    `metrics-core`,
    `metrics-client`,
    `metrics-server`,
    `trace-core`,
    `trace-client`,
    `trace-server`,
    tests,
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
  .jsSettings(sharedJsTestSettings)

lazy val `core-client` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core/client"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-core-client",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-dsl" % http4sV % Test
    ),
  )
  .jsSettings(sharedJsTestSettings)

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
  .jsSettings(sharedJsTestSettings)

lazy val `metrics-core` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("metrics/core"))
  .dependsOn(core)
  .settings(sharedTestSettings)
  .settings(
    name := s"$baseName-metrics-core",
    tlVersionIntroduced := Map("2.13" -> "0.20", "3" -> "0.20"),
    libraryDependencies += "org.typelevel" %%% "otel4s-core-metrics" % otel4sV,
  )
  .jsSettings(sharedJsTestSettings)

lazy val `metrics-client` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("metrics/client"))
  .dependsOn(`metrics-core`, `core-client`)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-metrics-client",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "otel4s-core" % otel4sV,
      "org.http4s" %%% "http4s-client" % http4sV,
      "org.typelevel" %%% "otel4s-semconv-metrics-experimental" % otel4sV % Test,
    ),
    tlVersionIntroduced := Map("2.13" -> "0.20", "3" -> "0.20"),
  )
  .jsSettings(sharedJsTestSettings)

lazy val `metrics-server` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("metrics/server"))
  .dependsOn(`metrics-core`, `core-server`)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-metrics-server",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "otel4s-core" % otel4sV,
      "org.http4s" %%% "http4s-server" % http4sV % Test,
      "org.typelevel" %%% "otel4s-semconv-metrics-experimental" % otel4sV % Test,
    ),
    tlVersionIntroduced := Map("2.13" -> "0.20", "3" -> "0.20"),
  )
  .jsSettings(sharedJsTestSettings)

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
      "org.http4s" %%% "http4s-client" % http4sV % Test,
    ),
  )
  .jsSettings(sharedJsTestSettings)

lazy val `trace-core` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("trace/core"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-trace-core"
  )
  .jsSettings(sharedJsTestSettings)

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
  .settings(
    // Formerly public setters remain checked after being hidden from Scala overload resolution.
    mimaBinaryApi ++= Seq(
      BinaryApi.keep[MethodNoLongerCheckedProblem](
        "org.http4s.otel4s.middleware.trace.client.ClientMiddleware#Builder.withErrorClassifier(org.http4s.otel4s.middleware.trace.ErrorClassifier)org.http4s.otel4s.middleware.trace.client.ClientMiddleware#Builder"
      ),
      BinaryApi.keep[MethodNoLongerCheckedProblem](
        "org.http4s.otel4s.middleware.trace.client.ClientMiddleware#Builder.withPerRequestTracingFilter(org.http4s.otel4s.middleware.trace.PerRequestFilter)org.http4s.otel4s.middleware.trace.client.ClientMiddleware#Builder"
      ),
      BinaryApi.keep[MethodNoLongerCheckedProblem](
        "org.http4s.otel4s.middleware.trace.client.ClientMiddleware#Builder.withPerRequestPropagationFilter(org.http4s.otel4s.middleware.trace.PerRequestFilter)org.http4s.otel4s.middleware.trace.client.ClientMiddleware#Builder"
      ),
    )
  )
  .jsSettings(sharedJsTestSettings)

lazy val `trace-server` = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("trace/server"))
  .dependsOn(`trace-core`, `core-server`)
  .settings(sharedSettings)
  .settings(
    name := s"$baseName-trace-server",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "org.http4s" %%% "http4s-server" % http4sV,
      "org.typelevel" %%% "otel4s-core-trace" % otel4sV,
    ),
  )
  .settings(
    // Formerly public setters remain checked after being hidden from Scala overload resolution.
    mimaBinaryApi ++= Seq(
      BinaryApi.keep[MethodNoLongerCheckedProblem](
        "org.http4s.otel4s.middleware.trace.server.ServerMiddleware#Builder.withErrorClassifier(org.http4s.otel4s.middleware.trace.ErrorClassifier)org.http4s.otel4s.middleware.trace.server.ServerMiddleware#Builder"
      ),
      BinaryApi.keep[MethodNoLongerCheckedProblem](
        "org.http4s.otel4s.middleware.trace.server.ServerMiddleware#Builder.withPerRequestTracingFilter(org.http4s.otel4s.middleware.trace.PerRequestFilter)org.http4s.otel4s.middleware.trace.server.ServerMiddleware#Builder"
      ),
      BinaryApi.keep[MethodNoLongerCheckedProblem](
        "org.http4s.otel4s.middleware.trace.server.ServerMiddleware#Builder.withPerRequestReversePropagationFilter(org.http4s.otel4s.middleware.trace.PerRequestFilter)org.http4s.otel4s.middleware.trace.server.ServerMiddleware#Builder"
      ),
    )
  )
  .jsSettings(sharedJsTestSettings)

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(
    `metrics-client` % "test->compile",
    `trace-client` % "test->compile",
    `metrics-server` % "test->compile",
    `trace-server` % "test->compile",
  )
  .settings(
    name := s"$baseName-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect-testkit" % catsEffectV % Test,
      "org.scalameta" %%% "munit" % munitV % Test,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectV % Test,
      "org.typelevel" %%% "otel4s-sdk-testkit" % otel4sSdkV % Test,
      "org.typelevel" %%% "otel4s-semconv-metrics-experimental" % otel4sV % Test,
    ),
  )
  .jsSettings(sharedJsTestSettings)

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
