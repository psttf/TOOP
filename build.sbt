import play.sbt.PlayImport._
import play.sbt.PlayScala

val scalaVersionValue = "2.12.6"
val defaultScalacOptions = Seq("-deprecation", "-encoding", "utf-8")

val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
val specs2_core = "org.specs2" %% "specs2-core" % "4.2.0"
val monix = "io.monix" %% "monix" % "2.3.3"
val macwire =  "com.softwaremill.macwire" %% "macros" % "2.3.0" % "provided"
val rollbar = "com.rollbar" % "rollbar-java" % "1.2.0"
val webjars_play = "org.webjars" %% "webjars-play" % "2.6.3"
val scalatestPlay = "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2"

//JS Libs
val bootstrap = "org.webjars" % "bootstrap" % "4.1.0"
val ace_builds = "org.webjars.bower" % "ace-builds" % "1.3.3"
val jquery = "org.webjars" % "jquery" % "3.3.1"

lazy val toopCore = (project in file("toop-core"))
  .settings(
    name := "toop-core",
    version := "1.0",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      parserCombinators,
      specs2_core % Test
    ),
    scalacOptions ++= defaultScalacOptions
  )

lazy val toopWeb = (project in file("toop-web"))
  .settings(
    name := "toop-web",
    version := "1.2",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      webjars_play,
      monix,
      macwire,
      rollbar,
      bootstrap,
      ace_builds,
      jquery,
      scalatestPlay % Test
    ),
    routesGenerator := InjectedRoutesGenerator,
    scalacOptions ++= defaultScalacOptions
  ).enablePlugins(PlayScala).dependsOn(toopCore)

lazy val toopCli = (project in file("toop-cli"))
  .settings(
    name := "toop-cli",
    version := "0.1.1",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.7.0",
    ),
    scalacOptions ++= defaultScalacOptions,
    Compile/mainClass := Some("cli.Main"),
    assemblyJarName in assembly := "sigmac.jar"
  )
  .dependsOn(toopCore)
