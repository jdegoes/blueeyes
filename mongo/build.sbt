name := "blueeyes-mongo"

version := "0.6.0-SNAPSHOT"

organization := "com.reportgrid"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

fork := true

libraryDependencies ++= Seq(
  "org.mongodb"                 % "mongo-java-driver"   % "2.7.3",
  "rhino"                       % "js"                  % "1.7R2",
  "org.specs2"                  %% "specs2"             % "1.8"            % "test",
  "org.mockito"                 % "mockito-all"         % "1.8.5"          % "test",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "test"
)

resolvers ++= Seq(
  "Sonatype Releases" at          "http://oss.sonatype.org/content/repositories/releases",
  "Maven Repo 1" at               "http://repo1.maven.org/maven2/"
)

parallelExecution in Test := false
