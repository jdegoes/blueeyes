name := "blueeyes"

version := "0.6.0-SNAPSHOT"

organization := "com.reportgrid"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1",
  "commons-codec"               % "commons-codec"       % "1.5",
  "joda-time"                   % "joda-time"           % "1.6.2",
  "net.lag"                     % "configgy"            % "2.0.0" intransitive(),
  "org.jboss.netty"             % "netty"               % "3.2.6.Final",
  "org.mongodb"                 % "mongo-java-driver"   % "2.7.2",
  "com.typesafe.akka"           % "akka-actor"          % "2.0-M1",
  "org.xlightweb"               % "xlightweb"           % "2.13.2",
  "rhino"                       % "js"                  % "1.7R2",
  "javolution"                  % "javolution"          % "5.5.1",
  "org.scalaz"                  %% "scalaz-core"        % "6.0.2",
  "com.weiglewilczek.slf4s"     %% "slf4s"              % "1.0.7",
  "org.specs2"                  %% "specs2"             % "1.7"            % "provided",
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
  "Nexus Scala Tools" at          "http://nexus.scala-tools.org/content/repositories/releases",
  "Maven Repo 1" at               "http://repo1.maven.org/maven2/",
  "Guiceyfruit Googlecode " at    "http://guiceyfruit.googlecode.com/svn/repo/releases/"
)

parallelExecution in Test := false

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.reportgrid.com/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"public-snapshots/") 
  else                                   Some("releases"  at nexus+"public-releases/")
}

credentials := Credentials(Path.userHome / ".ivy2" / ".rgcredentials") :: Nil
