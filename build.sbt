import sbt.{Defaults, _}
import Keys.{javacOptions, libraryDependencies, _}
import play.sbt.{PlayImport, PlayScala}
import play.sbt.PlayImport._
import PlayKeys._

val scalaVersionValue = "2.11.11"

val commons_io = "commons-io" % "commons-io" % "2.4"
val specs2_core = "org.specs2" %% "specs2-core" % "3.8.9" % "test"

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
			jdbc,
			cache,
			specs2_core
		),
		routesGenerator := StaticRoutesGenerator
	)
).enablePlugins(PlayScala,SbtWeb).dependsOn(toopCore)
