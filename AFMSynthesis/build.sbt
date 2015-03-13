import AssemblyKeys._

assemblySettings

jarName in assembly := "AFMSynthesis.jar"

name := "AFMSynthesis"

version := "0.1"

scalaVersion := "2.11.1"

libraryDependencies ++=  Seq(
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.9.0",
  "com.github.tototoshi" %% "scala-csv" % "1.2.1",
  "org.scalatest" %% "scalatest" % "2.2.1"
)

