import sbt._
class Plugins(info: ProjectInfo) extends PluginDefinition(info)
{
  val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
  val sbtIdea   = "com.github.mpeltonen"  % "sbt-idea-plugin" % "0.4.0-SNAPSHOT"
  
  val eclipsify = "de.element34"          % "sbt-eclipsify"   % "0.6.0"

  val netbeansPluginRepo = "Netbeans Plugin Github Repo" at "http://remeniuk.github.com/maven/"
  val netbeansPlugin = "org.netbeans.plugin" % "sbt-netbeans-plugin" % "0.0.7_0.7.7"

  val commonsIo = "commons-io"            % "commons-io"      % "2.0"  
}

