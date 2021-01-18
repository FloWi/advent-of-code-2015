import sbt.Keys._
import sbt._

//val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file(""))
  .settings(
    name := "de.flwi.adventofcode2015",
    version := "0.1.0",
    // scalacOptions ++= Seq(
    //   "-language:postfixOps",
    //   // "-Ykind-projector",
    //   //"-Yexplicit-nulls",
    //   "-source",
    //   "3.1"
    // ),
    scalaVersion := "2.13.4",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "com.lihaoyi" %% "fastparse" % "2.2.2",
      "org.typelevel" %% "cats-parse" % "0.2.0",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0",
      "io.circe" %% "circe-core" % "0.12.3",
      "io.circe" %% "circe-parser" % "0.12.3"
    )
  )
