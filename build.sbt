ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

publish / skip := true

lazy val juicer = project
  .in(file("."))
  .settings(
    name := "juicer",
    version := "0.0.1",
    scalaVersion := "3.3.0",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    libraryDependencies ++= Seq(
      "io.github.edadma" %% "char-reader" % "0.1.11",
      "io.github.edadma" %% "datetime" % "0.1.18",
      "io.github.edadma" %% "squiggly" % "0.1.16",
      "io.github.edadma" %% "yaml" % "0.1.12",
      "io.github.edadma" %% "commonmark" % "0.1.0-pre.20",
    ),
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "4.1.0",
      "com.lihaoyi" %% "pprint" % "0.8.1" /*% "test"*/,
      "org.ekrich" %% "sconfig" % "1.5.0",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
