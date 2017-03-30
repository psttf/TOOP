scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.4",
  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)

//libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

//libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators" % "1.0.4"	

//libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.5"

javacOptions ++= Seq("-encoding", "UTF-8")

//fork in run := true
