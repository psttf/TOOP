import play.sbt.PlayImport._
import play.sbt.PlayScala

val scalaVersionValue = "2.12.5"
val defaultScalacOptions = Seq("-deprecation", "-encoding", "utf-8")

val commons_io = "commons-io" % "commons-io" % "2.6"
val specs2_core = "org.specs2" %% "specs2-core" % "4.1.0" % "test"
val webjars_play = "org.webjars" %% "webjars-play" % "2.6.3"
val bootstrap = "org.webjars" % "bootstrap" % "3.3.7"
val ace_builds = "org.webjars.bower" % "ace-builds" % "1.2.6"
val jquery = "org.webjars" % "jquery" % "3.2.0"
val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

lazy val toopCore = (project in file("toop-core"))
  .settings(
    name := "toop-core",
    version := "1.0",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      commons_io,
      parserCombinators,
      specs2_core
    ),
    scalacOptions ++= defaultScalacOptions
  )

lazy val toopWeb = (project in file("toop-web"))
  .settings(
    name := "toop-web",
    version := "1.0",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      webjars_play,
      bootstrap,
      ace_builds,
      jquery,
      guice,
      specs2_core
    ),
    routesGenerator := InjectedRoutesGenerator,
    scalacOptions ++= defaultScalacOptions
  ).enablePlugins(PlayScala).dependsOn(toopCore)
