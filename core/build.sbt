name := "blueeyes-core"

version := "0.6.0-SNAPSHOT"

organization := "com.reportgrid"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1",
  "commons-codec"               % "commons-codec"       % "1.5",
  "joda-time"                   % "joda-time"           % "1.6.2",
  "org.jboss.netty"             % "netty"               % "3.2.6.Final",
  "org.xlightweb"               % "xlightweb"           % "2.13.2",
  "javolution"                  % "javolution"          % "5.5.1",
  "com.typesafe.akka"           % "akka-actor"          % "2.0",
  "org.streum"                  %% "configrity"         % "0.9.0",
  "com.weiglewilczek.slf4s"     %% "slf4s"              % "1.0.7",
  "org.specs2"                  %% "specs2"             % "1.8"            % "provided",
  "org.mockito"                 % "mockito-all"         % "1.8.5"          % "provided",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "provided",
  "javax.servlet"             % "javax.servlet-api"     % "3.0.1"          % "provided",
  "org.eclipse.jetty"           % "jetty-server"        % "8.1.0.RC5"      % "test",
  "org.eclipse.jetty"           % "jetty-servlet"       % "8.1.0.RC5"      % "test"
)


resolvers ++= Seq(
  "Typesafe Repository" at        "http://repo.typesafe.com/typesafe/releases/",
  "JBoss Releases" at             "http://repository.jboss.org/nexus/content/groups/public/",
  "Sonatype Releases" at          "http://oss.sonatype.org/content/repositories/releases",
  "Maven Repo 1" at               "http://repo1.maven.org/maven2/",
  "Guiceyfruit Googlecode " at    "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)

parallelExecution in Test := false
