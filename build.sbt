import play.sbt.PlayImport._
import play.sbt.PlayScala
import sbt.Keys.{libraryDependencies, _}
import sbt.{Defaults, _}

val scalaVersionValue = "2.11.11"

val commons_io = "commons-io" % "commons-io" % "2.5"
val specs2_core = "org.specs2" %% "specs2-core" % "3.8.9" % "test"
val webjars_play = "org.webjars" %% "webjars-play" % "2.5.0"
val bootstrap = "org.webjars" % "bootstrap" % "3.3.7"
val ace_builds = "org.webjars.bower" % "ace-builds" % "1.2.6"
val jquery = "org.webjars" % "jquery" % "3.2.0"

val toopCore = sbt.Project(
  id = "toop-core",
  base = file("toop-core"),
  settings = Defaults.defaultSettings ++ Seq(
    sbt.Keys.version := "1.0",
    sbt.Keys.scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      commons_io,
      specs2_core
    ),
    sbt.Keys.scalacOptions in Test ++= Seq("-Yrangepos"),
    sbt.Keys.resolvers ++=
      Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
  )
)

lazy val root = sbt.Project(
  id = "toop-web",
  base = file("toop-web"),
  settings = Seq(
    scalaVersion := scalaVersionValue,
    libraryDependencies ++= Seq(
      webjars_play,
      bootstrap,
      ace_builds,
      jquery,
      jdbc,
      cache,
      specs2_core
    ),
    routesGenerator := StaticRoutesGenerator
  )
).enablePlugins(PlayScala,SbtWeb).dependsOn(toopCore)
