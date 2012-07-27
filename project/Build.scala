import sbt._
import Keys._

object BlueEyesBuild extends Build {
  val nexusSettings : Seq[Project.Setting[_]] = Seq(
    resolvers ++= Seq("ReportGrid repo (public)"          at "http://nexus.reportgrid.com/content/repositories/public-releases",
                      "ReportGrid snapshot repo (public)" at "http://nexus.reportgrid.com/content/repositories/public-snapshots",
                      "Sonatype Jetty"                    at "https://oss.sonatype.org/content/groups/jetty/",
                      "Typesafe Repository"               at "http://repo.typesafe.com/typesafe/releases/",
                      "Sonatype Releases"                 at "http://oss.sonatype.org/content/repositories/releases",
                      "Sonatype Snapshots"                at "http://oss.sonatype.org/content/repositories/snapshots",
                      "JBoss Releases"                    at "http://repository.jboss.org/nexus/content/groups/public/",
                      "Maven Repo 1"                      at "http://repo1.maven.org/maven2/",
                      "Guiceyfruit Googlecode"            at "http://guiceyfruit.googlecode.com/svn/repo/releases/"),

    credentials += Credentials(Path.userHome / ".ivy2" / ".rgcredentials"),
    publishMavenStyle := true,
    pomIncludeRepository := { (repo: MavenRepository) => false },

    pomExtra :=
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
          <name>John De Goes</name>
        </developer>
        <developer>
          <id>nuttycom</id>
          <name>Kris Nuttycombe</name>
        </developer>
        <developer>
          <id>mlagutko</id>
          <name>Michael Lagutko</name>
        </developer>
        <developer>
          <id>dchenbecker</id>
          <name>Derek Chen-Becker</name>
        </developer>
      </developers>,

    publishTo <<= (version) { version: String =>
      val nexus = "http://nexus.reportgrid.com/content/repositories/"
      if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"public-snapshots/") 
      else                                   Some("releases"  at nexus+"public-releases/")
    }
  )

  val specs2Version = "1.11"

  val commonSettings = Seq(
    crossScalaVersions := Seq("2.9.1", "2.9.2"),

    version := "0.6.0-SNAPSHOT",

    organization := "com.reportgrid",

    libraryDependencies ++= Seq(
      "org.specs2"                  %  "specs2_2.9.1"       % specs2Version    % "test",
      "org.scala-tools.testing"     %  "scalacheck_2.9.1"   % "1.9"            % "test"
    ),

    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )

  lazy val blueeyes = Project(id = "blueeyes", base = file(".")).settings((nexusSettings ++ commonSettings): _*) aggregate(core, json, mongo)
  
  lazy val json  = Project(id = "json", base = file("json")).settings((nexusSettings ++ commonSettings): _*)

  // Core exposes some specs2 helpers, so we need a "provided" scope
  val coreSettings = Seq(
    libraryDependencies ++= Seq(
      "org.specs2"                  %  "specs2_2.9.1"       % specs2Version    % "provided"
    )
  )

  lazy val core  = Project(id = "core", base = file("core")).settings((nexusSettings ++ commonSettings ++ coreSettings): _*) dependsOn json

  lazy val mongo = Project(id = "mongo", base = file("mongo")).settings(nexusSettings : _*) dependsOn (core, json % "test->test")
  
  //lazy val actor = Project(id = "actor", base = file("actor")).settings(nexusSettings : _*)
}


// vim: set ts=4 sw=4 et:
