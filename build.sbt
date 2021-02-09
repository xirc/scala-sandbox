ThisBuild / name := "scala-sandbox"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.4"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)
