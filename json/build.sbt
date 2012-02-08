name := "blueeyes-json"

version := "0.6.0-SNAPSHOT"

organization := "com.reportgrid"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "commons-codec"               % "commons-codec"       % "1.5",
  "joda-time"                   % "joda-time"           % "1.6.2",
  "com.typesafe.akka"           % "akka-actor"          % "2.0-M1",
  "rhino"                       % "js"                  % "1.7R2",
  "javolution"                  % "javolution"          % "5.5.1",
  "org.scalaz"                  %% "scalaz-core"        % "7.0-SNAPSHOT" changing(),
  "com.weiglewilczek.slf4s"     %% "slf4s"              % "1.0.7",
  "org.specs2"                  %% "specs2"             % "1.8-SNAPSHOT"   % "provided" changing(),
  "org.mockito"                 % "mockito-all"         % "1.8.5"          % "provided",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "provided"
)

resolvers ++= Seq(
  "ReportGrid repo" at            "http://devci01.reportgrid.com:8081/content/repositories/releases",
  "ReportGrid snapshot repo" at   "http://devci01.reportgrid.com:8081/content/repositories/snapshots",
  "Scala-Tools Releases" at       "http://scala-tools.org/repo-releases/",
  "Scala-Tools Snapshots" at      "http://scala-tools.org/repo-snapshots/",
  "Akka Repository" at            "http://akka.io/repository/",
  "JBoss Releases" at             "http://repository.jboss.org/nexus/content/groups/public/",
  "Sonatype Releases" at          "http://oss.sonatype.org/content/repositories/releases",
  "Maven Repo 1" at               "http://repo1.maven.org/maven2/",
  "Guiceyfruit Googlecode " at    "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)
