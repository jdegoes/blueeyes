import com.jsuereth.pgp.sbtplugin.PgpKeys._

// Disable PGP for local builds
skip in pgpSigner := true

name := "blueeyes"

version := "0.5.3-SNAPSHOT"

organization := "com.reportgrid"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1",
  "commons-codec"               % "commons-codec"       % "1.5",
  "joda-time"                   % "joda-time"           % "1.6.2",
  "net.lag"                     % "configgy"            % "2.0.0" intransitive(),
  "org.jboss.netty"             % "netty"               % "3.2.6.Final",
  "org.mongodb"                 % "mongo-java-driver"   % "2.7.3",
  "se.scalablesolutions.akka"   % "akka-actor"          % "1.2",
  "se.scalablesolutions.akka"   % "akka-typed-actor"    % "1.2",
  "org.xlightweb"               % "xlightweb"           % "2.13.2",
  "rhino"                       % "js"                  % "1.7R2",
  "javolution"                  % "javolution"          % "5.5.1",
  "org.scalaz"                  %% "scalaz-core"        % "6.0.2",
  "com.weiglewilczek.slf4s"     %% "slf4s"              % "1.0.7",
  "org.specs2"                  %% "specs2"             % "1.8"   % "provided",
  "org.mockito"                 % "mockito-all"         % "1.8.5"          % "provided",
  "org.scala-tools.testing"     %% "scalacheck"         % "1.9"            % "provided"
)

resolvers ++= Seq(
  "ReportGrid repo" at            "http://nexus.reportgrid.com/content/repositories/releases",
  "ReportGrid snapshot repo" at   "http://nexus.reportgrid.com/content/repositories/snapshots",
  "Akka Repository" at            "http://repo.akka.io/releases/",
  "JBoss Releases" at             "http://repository.jboss.org/nexus/content/groups/public/",
  "Sonatype Releases" at          "http://oss.sonatype.org/content/repositories/releases",
  "Sonatype Snapshots" at         "http://oss.sonatype.org/content/repositories/snapshots",
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

publishMavenStyle := true

pomIncludeRepository := { (repo: MavenRepository) => false }

pomExtra :=
  <description>A lightweight Web 3.0 framework for Scala</description>
  <url>https://github.com/jdegoes/blueeyes</url>
  <licenses>
    <license>
      <name>MIT license</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:git@github.com/jdegoes/blueeyes.git</connection>
    <developerConnection>scm:git:git@github.com/jdegoes/blueeyes.git</developerConnection>
    <url>https://github.com/jdegoes/blueeyes</url>
  </scm>
  <developers>
    <developer>
      <id>jdegoes</id>
      <name>John DeGoes</name>
    </developer>
  </developers>