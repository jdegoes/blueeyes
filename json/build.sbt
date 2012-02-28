name := "blueeyes-json"

version := "0.6.0-SNAPSHOT"

organization := "com.reportgrid"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalaz"                  %% "scalaz-core"        % "7.0-SNAPSHOT" changing(),
  "joda-time"                   % "joda-time"           % "1.6.2"          % "optional",
  "org.specs2"                  %% "specs2"             % "1.8"            % "test",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "test"
)

resolvers ++= Seq(
  "ReportGrid repo" at            "http://devci01.reportgrid.com:8081/content/repositories/releases",
  "ReportGrid snapshot repo" at   "http://devci01.reportgrid.com:8081/content/repositories/snapshots",
  "Sonatype Releases" at          "http://oss.sonatype.org/content/repositories/releases"
)
