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

ThisBuild / crossScalaVersions := Seq("2.12.18", "2.13.12", "3.3.0")
ThisBuild / scalaVersion := "3.3.0"

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

val catsV = "2.10.0"
val catsEffectV = "3.5.0"
val fs2V = "3.7.0"
val http4sV = "0.23.19"
val fiberLocalV = "0.1.2"
val natchezV = "0.3.2"
val munitCatsEffectV = "2.0.0-M3"

val slf4jV    = "1.7.36"


// Projects
lazy val `natchez-http4s-otel` = tlCrossRootProject
  .aggregate(core, examples)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
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

      "io.chrisdavenport"           %%% "fiberlocal"           % fiberLocalV,
      "org.tpolecat"                %%% "natchez-core"         % natchezV,


      "org.typelevel"               %%% "munit-cats-effect"        % munitCatsEffectV         % Test,
      "org.tpolecat"                %%% "natchez-testkit"      % natchezV % Test,
    )
  ).jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule)},
  )

lazy val examples = project.in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(core.jvm)
  .settings(
    scalacOptions        -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "natchez-jaeger"      % natchezV,
      "org.http4s"   %% "http4s-dsl"          % http4sV,
      "org.http4s"   %% "http4s-ember-server" % http4sV,
      "org.http4s"   %% "http4s-ember-client" % http4sV,
      "org.slf4j"     % "slf4j-simple"        % slf4jV,
    )
  )

lazy val site = project.in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
