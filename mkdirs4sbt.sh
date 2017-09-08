
#!/bin/sh
mkdir -p src/{main,test}/{java,resources,scala}
mkdir lib project target

#create an initial build.sbt file

echo 'import Resolvers._
import Dependencies._
// factor out common settings into a sequence
lazy val buildSettings = Seq(
 organization := "com.andyr",
 version := "0.1.0",
 scalaVersion := "2.12.2"
)
// Sub-project specific dependencies
lazy val commonDeps = Seq(
 scalatest % Test
)
lazy val root = (project in file(".")).
  //aggregate(foo,bar).
  //dependsOn(foo,bar).
  settings(buildSettings: _*).
 settings(
 mainClass in assembly := Some("com.andyr.TestApp"),
  resolvers := oracleResolvers,
  libraryDependencies ++= commonDeps 
 )
//needed since seems like an issue with sbt run and loading a wav file
//so this will spawn a seprate jvm. one for sbt the other for the 
//running program. (I think)
fork in run := true
//lazy val foo = (project in file("Foo"))  
//lazy val bar = (project in file("Bar"))' > build.sbt  


#eclipse project creator
echo 'addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")' > project/plugins.sbt

echo 'import sbt._
import Keys._
// Shell prompt which show the current project and git branch
object ShellPromptPlugin extends AutoPlugin {
 override def trigger = allRequirements
 override lazy val projectSettings = Seq(
  shellPrompt := buildShellPrompt
 )
 val devnull: ProcessLogger = new ProcessLogger {
  def info (s: => String) {}
  def error (s: => String) { }
  def buffer[T] (f: => T): T = f
 }
 def currBranch =
  ("git status -sb" lines_! devnull headOption).
   getOrElse("-").stripPrefix("## ")
 val buildShellPrompt: State => String = {
  case (state: State) =>
   val currProject = Project.extract (state).currentProject.id
   s"""$currProject:$currBranch> """
 }
}' > project/ShellPrompPlugin.scala

echo 'import sbt._
import Keys._
object Resolvers {
 val sunrepo  = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
 val sunrepoGF = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish" 
 val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
 val oracleResolvers = Seq(sunrepo, sunrepoGF, oraclerepo)
}' > project/Resolvers.scala

echo 'import sbt._
import Keys._
object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.1"
}' > project/Dependencies.scala

echo 'package com.andyr
object TestApp {
    def main(args: Array[String]): Unit = {
    println("Hello")
}
}' > src/main/scala/app.scala
