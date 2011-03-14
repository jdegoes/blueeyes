import java.io.File
import sbt._
import scala.xml._

trait PublishingProject extends DefaultProject{
  override def managedStyle   = ManagedStyle.Maven
  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar  = defaultJarPath("-sources.jar")
  override def moduleID: String = normalizedName

  override def publishAction = task{

    val staleRepositories = NexusStagingList(info.projectPath, log)
    staleRepositories.foreach(NexusStagingDrop(info.projectPath, _, log))

    super.publishAction.run

    val repositories = NexusStagingList(info.projectPath, log)
    val result = repositories match{
      case x :: Nil =>{
        NexusStagingClose(info.projectPath, x, log)
        NexusStagingRelease(info.projectPath, x, log)

        val update = updateReadMe
        update.orElse{
          incrementVersionAction.run
          None
        }

        update
      }
      case x :: xs  => Some("There are more then one staging repostories: %s. Please, release manually.".format(repositories.mkString(", ")))
      case x        => Some("There are no staging repostories.")
    }
    result
  }

  private def updateReadMe = projectVersion.get match {
    case Some(value: BasicVersion) => {
      val readMeFile    = new File("README.md")

      FileUtilities.readString(readMeFile, log) match {
        case Left(e)              => Some(e)
        case Right(readMeContent) => {
          FileUtilities.write(readMeFile, readMeContent.replaceAll("<version>\\d+\\.\\d+\\.\\d+</version>", "<version>" + value.toString + "</version>"), log)

          log.info("README.md was updated.")

          None
        }
      }
    }
    case _ => None
  }

//  lazy val publishSnapshot = publishSnapshotAction
//  def publishSnapshotAction = task{
//    val oldVersion = projectVersion.get match
//    {
//      case Some(v: BasicVersion) =>
//      {
//        val newVersion = v.withExtra(Some("SNAPSHOT"))
//        projectVersion() = newVersion
//        log.info("Changing version to " + newVersion)
//        Some(v)
//      }
//      case _ => None
//    }
//
//    publishTo  = sonatypeSnapshots
//    val result = publishAction.run
//
//    publishTo  = sonatypeStaging
//    oldVersion.foreach(projectVersion() = _)
//
//    result
//  }

  val sourceArtifact  = Artifact.sources(artifactID)
  val docsArtifact    = Artifact.javadoc(artifactID)

  val sonatypeStaging   = "Sonatype Nexus Release Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  val sonatypeSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

  var publishTo = sonatypeStaging

  Credentials(info.projectPath  / "project" / "build" / "Sonatype.credentials", log)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)

  override def pomExtra =
    <parent>
      <groupId>org.sonatype.oss</groupId>
      <artifactId>oss-parent</artifactId>
      <version>5</version>
    </parent> ++
    <name>{name}</name> ++
    <description>A lightweight Web 3.0 framework for Scala</description> ++
    <url>http://github.com/jdegoes/blueeyes</url> ++
    <licenses>
      <license>
	<name>Apache 2</name>
	<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
	 <distribution>repo</distribution>
      </license>
    </licenses> ++
    <scm>
      <connection>scm:git:git@github.com:jdegoes/blueeyes.git</connection>
      <developerConnection>scm:git:git@github.com:jdegoes/blueeyes.git</developerConnection>
      <url>git@github.com:jdegoes/blueeyes.git</url>
    </scm> ++
    <developers></developers>

  override def pomPostProcess(pom: Node) =
    super.pomPostProcess(pom) match {
      case Elem(prefix, label, attr, scope, c @ _*) =>
        val children = c flatMap {
          case Elem(_, "repositories", _, _, repos @ _*) =>
            <profiles>
              <!-- poms deployed to maven central CANNOT have a repositories
                   section defined.  This download profile lets you
                   download dependencies other repos during development time. -->
              <profile>
                <id>download</id>
                <repositories>
                  {repos}
                </repositories>
              </profile>
            </profiles>
          case Elem(_, "dependencies", _, _, _ @ _*) =>
            // In SBT, parent projects depend on their children.  They should
            // not in Maven.
            None
          case x => x
        }
        Elem(prefix, label, attr, scope, children : _*)
    }
    override def deliverProjectDependencies = Nil
}