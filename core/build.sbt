name := "blueeyes-core"

libraryDependencies ++= Seq(
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1",
  "commons-codec"               % "commons-codec"       % "1.5",
  "joda-time"                   % "joda-time"           % "1.6.2",
  "org.jboss.netty"             % "netty"               % "3.2.6.Final",
  "org.xlightweb"               % "xlightweb"           % "2.13.2",
  "javolution"                  % "javolution"          % "5.5.1",
  "com.typesafe.akka"           % "akka-actor"          % "2.0",
  "org.streum"                  %  "configrity_2.9.1"   % "0.9.0",
  "com.weiglewilczek.slf4s"     %  "slf4s_2.9.1"        % "1.0.7",
  "org.specs2"                  %  "specs2_2.9.1"       % "1.8"            % "provided",
  "org.mockito"                 %  "mockito-all"        % "1.9.0"          % "test",
  "org.scala-tools.testing"     %  "scalacheck_2.9.1"   % "1.9"            % "test"
)

resolvers ++= Seq(
  "JBoss Releases" at             "http://repository.jboss.org/nexus/content/groups/public/",
  "Sonatype Releases" at          "http://oss.sonatype.org/content/repositories/releases",
  "Maven Repo 1" at               "http://repo1.maven.org/maven2/",
  "Guiceyfruit Googlecode " at    "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)

parallelExecution in Test := false


