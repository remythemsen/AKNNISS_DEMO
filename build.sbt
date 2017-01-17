name := "AKNNISS_DEMO"

organization := "dk.aknniss"

version := "1.0"

homepage := Some(url("https://github.com/remythemsen/AKNNISS"))

startYear := Some(2016)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/remythemsen/AKNNISS"),
    "scm:git:https://github.com/remythemsen/AKNNISS.git",
    Some("scm:git:git@github.com:remythemsen/AKNNISS.git")
  )
)

scalaVersion := "2.11.8"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq( jdbc , cache , ws   , specs2 % Test )

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.14",
  "com.typesafe.akka" %% "akka-remote" % "2.4.14",
  "com.typesafe" % "config" % "1.2.0",
  "com.github.romix.akka" %% "akka-kryo-serialization" % "0.5.0",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "com.acme.common" % "commonclass" % "1.0" from "file:///home/remeeh/school/thesis/AKNNISS/out/artifacts/LSH_jar/LSH.jar",
  "com.acme.common" % "commonclass" % "1.0" from "file:///home/remeeh/school/thesis/AKNNISS/out/artifacts/utils_jar/utils.jar"
)

maintainer := "Remy Themsen <remt@itu.dk>"

dockerRepository := Some("remeeh")

dockerExposedPorts := Seq(9000,9090)

routesGenerator := InjectedRoutesGenerator

//dockerBaseImage := "java"

lazy val `aknniss_demo` = (project in file(".")).enablePlugins(PlayScala, JavaAppPackaging)
