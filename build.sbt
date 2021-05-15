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
val ScalaTestVersion = "3.2.8"
val CatsVersion = "2.6.0"
val ShapelessVersion = "2.3.6"

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % ScalaParallelCollectionsVersion,
      "org.scalactic" %% "scalactic" % ScalaTestVersion,
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test
    )
  )

lazy val scalaWithCats = (project in file("scala-with-cats"))
  .settings(
    name := "scala-with-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test
    )
  )

lazy val shapelessGuide = (project in file("shapeless-guide"))
  .settings(
    name := "shapeless-guide",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % ShapelessVersion,
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test
    )
  )
