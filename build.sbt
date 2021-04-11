name := "scala-sandbox"
version := "0.1"
scalaVersion := "2.13.5"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Werror"
)

ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.2",
  "org.scalactic" %% "scalactic" % "3.2.7",
  "org.scalatest" %% "scalatest" % "3.2.7" % "test"
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
