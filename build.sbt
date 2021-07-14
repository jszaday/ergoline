name := "ergoline"
organization := "edu.illinois.cs.ergoline"
version := "0.1"
scalaVersion := "2.13.6"

scalacOptions += "-feature"
scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.8"

Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)
Test / testOptions += Tests.Argument("-oD")

enablePlugins(Antlr4Plugin)
Antlr4 / antlr4PackageName := Some("edu.illinois.cs.ergoline")
Antlr4 / antlr4GenListener := false
Antlr4 / antlr4GenVisitor := true

// specify the name of the resulting jar
assembly / assemblyOutputPath := baseDirectory.value / "ergc.jar"
// skip the test during assembly
assembly / test := {}
