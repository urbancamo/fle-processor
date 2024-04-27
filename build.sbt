ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

scalacOptions += "-rewrite"
scalacOptions += "-source"
scalacOptions += "3.4-migration"

lazy val root = (project in file("."))
  .settings(
    name := "fle-processor"
  )
