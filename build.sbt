name := "AdventOfCode2018"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++=
  ("org.scalatest" %% "scalatest" % "3.0.5" % Test) ::
  ("com.typesafe.scala-logging" %% "scala-logging" % "3.9.0") ::
  ("ch.qos.logback" % "logback-classic" % "1.2.3") ::
  ("org.scalaj" %% "scalaj-http" % "2.4.1") :: Nil