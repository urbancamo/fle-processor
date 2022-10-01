ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "fle-processor"
  )
