name := "ergoline"
organization := "edu.illinois.cs.ergoline"
version := "0.1"
scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

enablePlugins(Antlr4Plugin)
antlr4PackageName in Antlr4 := Some("edu.illinois.cs.ergoline")
antlr4GenListener in Antlr4 := false
antlr4GenVisitor in Antlr4 := true
