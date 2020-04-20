import play.sbt.PlayImport._
import play.sbt.PlayScala

val scalaVersionValue = "2.12.10"
val defaultScalacOptions = Seq("-deprecation", "-encoding", "utf-8")

val cats = "org.typelevel" %% "cats-effect" % "2.0.0"
val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
val specs2_core = "org.specs2" %% "specs2-core" % "4.2.0"
val monix = "io.monix" %% "monix" % "2.3.3"
val webjars_play = "org.webjars" %% "webjars-play" % "2.6.3"
val bootstrap = "org.webjars" % "bootstrap" % "4.1.0"
val ace_builds = "org.webjars.bower" % "ace-builds" % "1.3.3"
val jquery = "org.webjars" % "jquery" % "3.3.1"
val playCirce = "com.dripower" %% "play-circe" % "2611.0"
val circeGeneric = "io.circe" %% "circe-generic" % "0.10.0"
val http4s = Seq(
  "io.circe" %% "circe-core" % "0.11.1",
  "io.circe" %% "circe-generic" % "0.11.1",
  "io.circe" %% "circe-parser" % "0.11.1",
  "org.http4s" %% "http4s-blaze-server" % "0.20.13",
  "org.http4s" %% "http4s-circe" % "0.20.13",
  "org.http4s" %% "http4s-dsl" % "0.20.13",
  "org.http4s" %% "http4s-blaze-client" % "0.20.13",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

lazy val toopCore = (project in file("toop-core"))
  .settings(
    name := "toop-core",
    version := "1.0",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      cats,
      parserCombinators,
      specs2_core % Test
    ),
    scalacOptions ++= defaultScalacOptions
  )

lazy val toopWeb = (project in file("toop-web"))
  .settings(
    name := "toop-web",
    version := "1.1",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      webjars_play,
      monix,
      bootstrap,
      ace_builds,
      jquery,
      guice,
      playCirce,
      circeGeneric,
      specs2_core % Test
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

lazy val toopHTTP4S = (project in file("toop-http4s"))
  .settings(
    name := "toop-http4s",
    version := "0.0.1",
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= http4s,
    scalacOptions ++= defaultScalacOptions :+ "-Ypartial-unification",
    Compile/mainClass := Some("server.Main")
  )
  .dependsOn(toopCore)
