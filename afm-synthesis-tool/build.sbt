import play.PlayScala

name := """afm-synthesis-tool"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" % "angularjs" % "1.3.0",
  "org.webjars" % "font-awesome" % "4.3.0-1",
  "com.github.tototoshi" %% "scala-csv" % "1.2.1",
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.9.0",
  "com.github.tototoshi" %% "scala-csv" % "1.2.1",
  "org.scalatest" %% "scalatest" % "2.2.1",
  jdbc,
  anorm,
  cache,
  ws
)