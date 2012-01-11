import sbt._
import Keys._
import sbtassembly.Plugin.AssemblyKeys._
import sbt.NameFilter._

object BlueEyesBuild extends Build {
  val scalaz = com.samskivert.condep.Depends(
    ("scalaz", "core", "org.scalaz"                  %% "scalaz-core"        % "7.0-SNAPSHOT")
  )

  val nexusSettings : Seq[Project.Setting[_]] = Seq(
    resolvers ++= Seq("ReportGrid repo"          at   "http://nexus.reportgrid.com/content/repositories/releases",
                      "ReportGrid repo (public)" at   "http://nexus.reportgrid.com/content/repositories/public-releases",
                      "ReportGrid snapshot repo"          at   "http://nexus.reportgrid.com/content/repositories/snapshots",
                      "ReportGrid snapshot repo (public)" at   "http://nexus.reportgrid.com/content/repositories/public-snapshots"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".rgcredentials"),
    publishTo <<= (version) { version: String =>
      val nexus = "http://nexus.reportgrid.com/content/repositories/"
      if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"public-snapshots/") 
      else                                   Some("releases"  at nexus+"public-releases/")
    }
  )

  lazy val blueeyes = Project(id = "blueeyes", base = file(".")) aggregate(core, json, mongo)
  
  val jsonSettings = nexusSettings ++ Seq(libraryDependencies ++= scalaz.libDeps)
  lazy val json  = scalaz.addDeps(Project(id = "json", base = file("json")).settings(jsonSettings : _*))

  lazy val core  = Project(id = "core", base = file("core")).settings(nexusSettings : _*) dependsOn json

  lazy val mongo = Project(id = "mongo", base = file("mongo")).settings(nexusSettings : _*) dependsOn (core, json % "test->test")
  
  lazy val actor = Project(id = "actor", base = file("actor")).settings(nexusSettings : _*)
}


// vim: set ts=4 sw=4 et:
