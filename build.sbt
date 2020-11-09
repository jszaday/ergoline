name := "ergoline"

version := "0.1"

scalaVersion := "2.13.3"

Compile / scalaSource := baseDirectory.value / "src"
Compile / javaSource := baseDirectory.value / "src"

// https://mvnrepository.com/artifact/org.antlr/antlr4-runtime
libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.8"
