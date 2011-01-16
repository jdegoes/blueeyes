import sbt._
class Plugins(info: ProjectInfo) extends PluginDefinition(info)
{
  val eclipsify = "de.element34"          % "sbt-eclipsify"   % "0.6.0"
  val sbtIdea   = "com.github.mpeltonen"  % "sbt-idea-plugin" % "0.2.0"
}

