import com.typesafe.tools.mima.core._

ThisBuild / tlBaseVersion := "0.2" // your current series x.y

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

val Scala213 = "2.13.8"

ThisBuild / crossScalaVersions := Seq("2.12.15", Scala213, "3.1.2")
ThisBuild / scalaVersion := Scala213

ThisBuild / testFrameworks += new TestFramework("munit.Framework")

val catsV = "2.7.0"
val catsEffectV = "3.3.11"
val fs2V = "3.2.7"
val http4sV = "0.23.12"
val openTelemetryV = "1.22.0"
val otel4sV = "0.0-b5c5056-SNAPSHOT"
val munitCatsEffectV = "1.0.7"

val slf4jV    = "1.7.30"


// Projects
lazy val `natchez-http4s-otel` = tlCrossRootProject
  .aggregate(core, examples)

lazy val core = crossProject(JVMPlatform, JSPlatform)
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

      "io.chrisdavenport"           %%% "fiberlocal"           % "0.1.1",
      "org.typelevel"               %%% "otel4s-core-trace"    % otel4sV,


      "org.typelevel"               %%% "munit-cats-effect-3"        % munitCatsEffectV         % Test,

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
      "org.typelevel"    %% "otel4s-java" % otel4sV,
      "io.opentelemetry" % "opentelemetry-exporter-otlp" % openTelemetryV % Runtime,
      "io.opentelemetry" % "opentelemetry-sdk-extension-autoconfigure" % s"${openTelemetryV}-alpha" % Runtime,
      "org.http4s"   %% "http4s-dsl"          % http4sV,
      "org.http4s"   %% "http4s-ember-server" % http4sV,
      "org.http4s"   %% "http4s-ember-client" % http4sV,
      "org.slf4j"     % "slf4j-simple"        % slf4jV,
    ),
    run / fork := true,
    javaOptions += "-Dotel.java.global-autoconfigure.enabled=true"
  )

lazy val site = project.in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm)
