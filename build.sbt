ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "begin"
  )

val hedgehogVersion = "0.10.0"

//libraryDependencies ++= Seq(
//  "qa.hedgehog" %% "hedgehog-core" % hedgehogVersion,
//  "qa.hedgehog" %% "hedgehog-runner" % hedgehogVersion,
//  "qa.hedgehog" %% "hedgehog-sbt" % hedgehogVersion
//).map(_ % Test)
//testFrameworks += TestFramework("hedgehog.sbt.Framework")

libraryDependencies += "qa.hedgehog" %% "hedgehog-munit" % hedgehogVersion
testFrameworks += TestFramework("munit.runner.Framework")

