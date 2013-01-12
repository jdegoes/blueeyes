import sbt._
import Keys._

object BlueEyesBuild extends Build {
  val nexusSettings : Seq[Project.Setting[_]] = Seq(
    resolvers ++= Seq(
      "ReportGrid repo (public)"          at "http://nexus.reportgrid.com/content/repositories/public-releases",
      "ReportGrid snapshot repo (public)" at "http://nexus.reportgrid.com/content/repositories/public-snapshots",
      "Sonatype Jetty"                    at "http://oss.sonatype.org/content/groups/jetty/",
      "Typesafe Repository"               at "http://repo.typesafe.com/typesafe/releases/",
      "Sonatype Releases"                 at "http://oss.sonatype.org/content/repositories/releases",
      "Sonatype Snapshots"                at "http://oss.sonatype.org/content/repositories/snapshots",
      "JBoss Releases"                    at "http://repository.jboss.org/nexus/content/groups/public/",
      "Maven Repo 1"                      at "http://repo1.maven.org/maven2/",
      "Guiceyfruit Googlecode"            at "http://guiceyfruit.googlecode.com/svn/repo/releases/"
    ),

    credentials += Credentials(Path.userHome / ".ivy2" / ".rgcredentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
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

  val specs2Version = "1.12.3"

  val commonSettings = Seq(
    scalaVersion := "2.9.2",

    crossScalaVersions := Seq("2.9.2"),

    version := "1.0.0-M6.7",

    organization := "com.reportgrid",

    libraryDependencies ++= Seq(
      "org.scalaz"         %% "scalaz-core"  % "7.0-precog-M1",
      "org.specs2"         %% "specs2"       % specs2Version    % "test" changing(),
      "org.scalacheck"     %% "scalacheck"   % "1.10.0"         % "test"
    ),

    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )

  lazy val blueeyes = Project(id = "blueeyes", base = file(".")).settings((nexusSettings ++ commonSettings): _*) aggregate(util, json, akka_testing, bkka, core, test, mongo)
  
  lazy val util  = Project(id = "util", base = file("util")).settings((nexusSettings ++ commonSettings): _*)

  lazy val json  = Project(id = "json", base = file("json")).settings((nexusSettings ++ commonSettings): _*)

  lazy val akka_testing  = Project(id = "akka_testing", base = file("akka_testing")).settings((nexusSettings ++ commonSettings): _*) dependsOn util

  lazy val bkka  = Project(id = "bkka", base = file("bkka")).settings((nexusSettings ++ commonSettings): _*) dependsOn util

  lazy val core  = Project(id = "core", base = file("core")).settings((nexusSettings ++ commonSettings): _*) dependsOn (util, json, bkka, akka_testing)

  lazy val test  = Project(id = "test", base = file("test")).settings((nexusSettings ++ commonSettings): _*) dependsOn core

  lazy val mongo = Project(id = "mongo", base = file("mongo")).settings((nexusSettings ++ commonSettings): _*) dependsOn (core, json % "test->test")

  //lazy val actor = Project(id = "actor", base = file("actor")).settings(nexusSettings : _*)
}


// vim: set ts=4 sw=4 et:
