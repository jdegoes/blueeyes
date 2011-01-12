import _root_.sbt._
import Process._
import scala.xml._

trait GpgPlugin extends BasicManagedProject {
  def gpgCommand: String = "gpg"

  lazy val sign = signAction

  val signaturesConfig = config("signatures")

  override def artifacts = super.artifacts flatMap { artifact =>
        val ascArtifact = Artifact(artifact.name, "asc", artifact.extension+".asc", artifact.classifier, Seq(signaturesConfig), None)
        Seq(artifact, ascArtifact)
      }

  def signAction = signTask(artifacts)
    .dependsOn(makePom)
    .describedAs("Signs each artifact with GnuPG.")

  def signTask(artifacts: Iterable[Artifact]): Task = task {
    artifacts.toStream flatMap signArtifact firstOption
  }

  def signArtifact(artifact: Artifact): Option[String] = {
    val path = artifact2Path(artifact)
    path.ext match {
      case "asc" => None
      case "md5" => None
      case "sha1" => None
      case _ => ExternalProcess(List(gpgCommand, "--passphrase-fd", "0", "-ab", "--yes", path.asFile.getAbsolutePath), Some(GpgProperties.password + "\n"), None, log)
            None
    }
  }

  protected def artifact2Path(artifact: Artifact): Path = {
    val filename =
      artifactBaseName +
      (artifact.classifier map { "-"+_ } getOrElse "") +
      "."+artifact.extension
    outputPath / filename
  }

  override def deliverLocalAction = super.deliverLocalAction dependsOn(sign)
  override def deliverAction = super.deliverAction dependsOn(sign)

  override def pomPostProcess(pom: Node): Node =
    pom match {
      case Elem(prefix, label, attr, scope, c @ _*) =>
        val children = c flatMap {
          case Elem(_, "packaging", _, _, _ @ _*) =>
            <packaging>{mainArtifactType}</packaging>
          case x => x
        }
        Elem(prefix, label, attr, scope, children : _*)
    }

  def mainArtifactType: String =
    this match {
      case proj: BasicScalaProject => proj.mainArtifact.`type`
      case _: ParentProject => "pom"
    }
}
