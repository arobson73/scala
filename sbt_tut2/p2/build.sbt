import Resolvers._
import Dependencies._

// factor out common settings into a sequence
lazy val buildSettingsp2 = Seq(
 organization := "com.andyr.p2",
 version := "0.1.0",
 scalaVersion := "2.12.1"
)

// Sub-project specific dependencies
lazy val commonDepsp2 = Seq(
 scalatest % Test
)

lazy val rootp2 = (project in file(".")).
  aggregate(p1).
  dependsOn(p1).
  settings(buildSettingsp2: _*).
 settings(
  resolvers := oracleResolvers,
  libraryDependencies ++= commonDepsp2 
 )

//needed since seems like an issue with sbt run and loading a wav file
//so this will spawn a seprate jvm. one for sbt the other for the 
//running program. (I think)
//fork in run := true


//lazy val p1 = (project in file("../p1"))  
lazy val p1 =  RootProject(file("../p1"))  
//lazy val bar = (project in file("Bar"))
