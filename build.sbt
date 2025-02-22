ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

ThisBuild / organization := "adventofcode2024"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2024",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.1.0",
      "guru.nidi" % "graphviz-java" % "0.18.1",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
    ),
  )
