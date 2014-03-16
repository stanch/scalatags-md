name := "scalatags-md"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "Typesafe" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  compilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),
  "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "com.scalatags" %% "scalatags" % "0.2.4",
  "org.pegdown" % "pegdown" % "1.4.2",
  "org.scalatest" %% "scalatest" % "2.1.0" % "test"
)
