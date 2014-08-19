import AssemblyKeys._

assemblySettings

jarName in assembly := "AFMSynthesis.jar"

name := "AFMSynthesis"

version := "0.1"

scalaVersion := "2.10.4"

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.0"

