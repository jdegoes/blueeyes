import sbt._

class BlueEyesProject(info: ProjectInfo) extends ParentProject(info) {
  trait Repositories {
    val scalareleases   = "Scala Repo Releases"        at "http://scala-tools.org/repo-releases/"
    val scalasnapshots  = "Scala-tools.org Repository" at "http://scala-tools.org/repo-snapshots/"
    val jbossreleases   = "JBoss Releases"             at "http://repository.jboss.org/nexus/content/groups/public/"
    val sonatyperelease = "Sonatype Releases"          at "http://oss.sonatype.org/content/repositories/releases"
  }
  
  class TestDepsProject(info: ProjectInfo) extends DefaultProject(info) with Repositories{
    val scalatest  = "org.scalatest"           % "scalatest"        % "1.2"    % "test"
    val scalaspec   = "org.scala-tools.testing"    % "specs_2.8.0"      % "1.6.6-SNAPSHOT"  % "test"
    val scalacheck  = "org.scala-tools.testing"    % "scalacheck_2.8.0" % "1.8-SNAPSHOT"  % "test"
    val mockito    = "org.mockito"             % "mockito-all"      % "1.8.4"  % "test"
  }

  // hack to work around failure of inheriting dependencies via traits
  lazy val testDeps = project("test-deps", "test-deps",  new TestDepsProject(_))

  // Blueeyes projects
  lazy val health = project("health", "health",  new HealthProject(_), testDeps)
  
  lazy val core = project("core", "core", new CoreProject(_), testDeps)

  class HealthProject(info: ProjectInfo) extends DefaultProject(info) with Repositories {
  }

  class JsonProject(info: ProjectInfo) extends DefaultProject(info) with Repositories {
    val paranamer   = "com.thoughtworks.paranamer" % "paranamer"            % "2.0"
    val junit      = "junit" % "junit" % "4.5" % "test"
  }
  
  class CoreProject(info: ProjectInfo) extends DefaultProject(info) with Repositories {
    val netty = "org.jboss.netty" % "netty" % "3.2.2.Final" % "compile"
    val async = "com.ning" % "async-http-client" % "1.1.0" % "compile"
  }
}
