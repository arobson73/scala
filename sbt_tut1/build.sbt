import Resolvers._
import Dependencies._

// factor out common settings into a sequence
lazy val buildSettings = Seq(
 organization := "com.andyr",
 version := "0.1.0",
 scalaVersion := "2.12.1"
)

// Sub-project specific dependencies
lazy val commonDeps = Seq(
 scalatest % Test
)

lazy val root = (project in file(".")).
  aggregate(foo,bar).
  dependsOn(foo,bar).
  settings(buildSettings: _*).
 settings(
  resolvers := oracleResolvers,
  libraryDependencies ++= commonDeps 
 )

lazy val foo = (project in file("Foo"))  
lazy val bar = (project in file("Bar"))  


