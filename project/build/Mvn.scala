import java.io.{InputStream, ByteArrayOutputStream, IOException}
import sbt._
import Process._
import util.matching.Regex

object ExtermalProcess{
  def apply(command: List[String], input: Option[String], log: Logger) ={

    val outputStream = new ByteArrayOutputStream
    val errorStream  = new ByteArrayOutputStream

    val process = command run (new ProcessIO(writeTo(input), pumpStream(outputStream, log) _, pumpStream(errorStream, log) _ ))

    if (process.exitValue != 0){
      throw new Exception("Process exitValue=" + process.exitValue)
    }

    if (errorStream.size() != 0) log.error(new String(errorStream.toByteArray))

    val output = new String(outputStream.toByteArray)

    log.info(output)

    output
  }

  private def pumpStream(byteArray: ByteArrayOutputStream, log: Logger)(processOutput: InputStream){

    try {
      val buffer = new Array[Byte](1024)
      var bytesRead: Int = 0
      do {
        bytesRead = processOutput.read(buffer)
        if (bytesRead > -1) {
          byteArray.write(buffer, 0, bytesRead)
        }
      } while (bytesRead > -1)
    }
    catch {
      case e: IOException => {
        log.error("Ignoring exception while reading output of process: " + e)
      }
    }
  }

  private def writeTo(input: Option[String])(stream: java.io.OutputStream) {
    input.foreach(v => {
      stream.write(v.getBytes())
//      stream.write("\n".getBytes())
      stream.flush()
    })
  }  
}

object Mvn {
  
  def apply(projectRoot: Path, input: Option[String], log: Logger, command: String, mvmArgs: String*): String = {

    val args = List("mvn", "-s", (projectRoot / "project" / "build" / "mvn.xml").asFile.getAbsolutePath, command, "-Dnexus.url=https://oss.sonatype.org", "-Dnexus.username=blueeyes", "-Dnexus.password=7rave5") ++ mvmArgs

    ExtermalProcess(args, input, log)
  }
}

object NexusStagingList {
  def apply(projectRoot: Path, log: Logger) = {

    log.info("Getting Nexus staging repositories list.")

    val output = Mvn(projectRoot, None, log, "nexus:staging-list")

    val regexp = new Regex("(comgithubblueeyes-\\d+)")

    regexp.findAllIn(output).toList.removeDuplicates
  }
}

object NexusStagingClose {
  def apply(projectRoot: Path, repositoryId: String, log: Logger) = {

    log.info("Closing staging repository: %s.".format(repositoryId))

    Mvn(projectRoot, None, log, "nexus:staging-close", "-Dnexus.repositoryId=" + repositoryId, "-Dnexus.description=Closing repository " + repositoryId)
  }
}

object NexusStagingRelease {
  def apply(projectRoot: Path, repositoryId: String, log: Logger) = {

    log.info("Releasing staging repository: %s.".format(repositoryId))

    Mvn(projectRoot, Some("releases"), log, "nexus:staging-release", "-Dnexus.repositoryId=" + repositoryId, "-Dnexus.description=repository release: " + repositoryId)
  }
}