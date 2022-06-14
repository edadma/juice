ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val juicer = crossProject( /*JSPlatform, */ JVMPlatform /*, NativePlatform*/ )
  .in(file("."))
  .settings(
    name := "juicer",
    version := "0.1.0",
    scalaVersion := "2.13.8",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
        "-Xasync"
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "cross-platform" % "0.1.1",
      "io.github.edadma" %%% "char-reader" % "0.1.7",
      "io.github.edadma" %%% "datetime" % "0.1.11",
      "io.github.edadma" %%% "squiggly" % "0.1.16-pre.21",
      "io.github.edadma" %%% "yaml" % "0.1.11",
      "io.github.edadma" %%% "commonmark" % "0.1.0-pre.19"
    ),
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.0.1",
//      "com.lihaoyi" %%% "pprint" % "0.6.6",
//      "org.parboiled" %%% "parboiled" % "2.3.0",
      "org.ekrich" %%% "sconfig" % "1.4.4"
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided",
  )
/*.nativeSettings(
    nativeLinkStubs := true,
    libraryDependencies += "org.ekrich" %%% "sjavatime" % "1.1.5"
  )*/
/*.jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.ekrich" %%% "sjavatime" % "1.1.5"
  )*/
