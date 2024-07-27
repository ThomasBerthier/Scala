ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val root = (project in file("."))
  .settings(
    name := "PredateurProie"
  )

libraryDependencies += "org.scalafx" %% "scalafx" % "22.0.0-R33"