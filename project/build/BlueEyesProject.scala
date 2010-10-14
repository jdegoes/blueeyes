import sbt._

class BlueEyesProject(info: ProjectInfo) extends DefaultProject(info)  with Repositories {

  val scalatest   = "org.scalatest"               % "scalatest"         % "1.2-for-scala-2.8.0.final-SNAPSHOT"    % "test"
  val scalaspec   = "org.scala-tools.testing"     % "specs_2.8.0"       % "1.6.5"       % "test"
  val scalacheck  = "org.scala-tools.testing"     % "scalacheck_2.8.0"  % "1.7"         % "test"
  val mockito     = "org.mockito"                 % "mockito-all"       % "1.8.4"       % "test"
  val paranamer   = "com.thoughtworks.paranamer"  % "paranamer"         % "2.0"
  val junit       = "junit"                       % "junit"             % "4.7"         % "test"
  val netty       = "org.jboss.netty"             % "netty"             % "3.2.2.Final" % "compile"
  val async       = "com.ning"                    % "async-http-client" % "1.1.0"       % "compile"
  val mongo       = "org.mongodb"                 % "mongo-java-driver" % "2.1"         % "compile"

  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar= defaultJarPath("-sources.jar")
  
  val sourceArtifact = Artifact.sources(artifactID)
  val docsArtifact = Artifact.javadoc(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)

  override def pomExtra =
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
}

trait Repositories {
  val scalareleases   = MavenRepository("Scala Repo Releases",        "http://scala-tools.org/repo-releases/")
  val scalasnapshots  = MavenRepository("Scala-tools.org Repository", "http://scala-tools.org/repo-snapshots/")
  val jbossreleases   = MavenRepository("JBoss Releases",             "http://repository.jboss.org/nexus/content/groups/public/")
  val sonatyperelease = MavenRepository("Sonatype Releases",          "http://oss.sonatype.org/content/repositories/releases")
  val nexusscalatools = MavenRepository("Nexus Scala Tols",           "http://nexus.scala-tools.org/content/repositories/releases")
}
