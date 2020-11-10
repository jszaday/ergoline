name := "ergoline"

version := "0.1"

scalaVersion := "2.13.3"

Compile / scalaSource := baseDirectory.value / "src/main/scala"
Test / scalaSource := baseDirectory.value / "src/test/scala"
Compile / javaSource := baseDirectory.value / "src/main/java"

// https://mvnrepository.com/artifact/org.antlr/antlr4-runtime
libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test