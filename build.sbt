import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

val scalaTest = "org.scalatest" %% "scalatest" % "3.2.12" % "test"
lazy val root = (project in file("."))
  .settings(
    name := "scala-coding-exercises",
    libraryDependencies += scalaTest
  )

