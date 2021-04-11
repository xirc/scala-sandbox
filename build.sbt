ThisBuild / name := "scala-sandbox"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.5"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Werror"
)

addCommandAlias(
  name = "ciFormat",
  Seq(
    "scalafmtSbt",
    "scalafmtAll"
  ).mkString(";")
)

addCommandAlias(
  "ciCheck",
  Seq(
    "clean",
    "scalafmtSbtCheck",
    "scalafmtCheckAll",
    "Test / compile",
    "test"
  ).mkString(";")
)

val ScalaParallelCollectionsVersion = "1.0.2"
val ScalaTestVersion = "3.2.7"

ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % ScalaParallelCollectionsVersion,
  "org.scalactic" %% "scalactic" % ScalaTestVersion,
  "org.scalatest" %% "scalatest" % ScalaTestVersion % "test"
)
