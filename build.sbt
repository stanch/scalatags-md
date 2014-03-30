name := "scalatags-md"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  compilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M6" cross CrossVersion.full),
  "org.scalamacros" % "quasiquotes" % "2.0.0-M6" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "com.scalatags" %% "scalatags" % "0.2.4",
  "org.planet42" %% "laika-core" % "0.5.0",
  "org.scalatest" %% "scalatest" % "2.1.0" % "test"
)
