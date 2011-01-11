import sbt._
import FileUtilities._
import java.io.File

trait CoverageProject extends BasicManagedProject{
  private val processor      = "coverage is com.proinnovate sbt-coverage 0.1" // -SNAPSHOT"
  private val processorRepo  = "undercoverRepo at http://undercover.googlecode.com/svn/maven/repository/"

  override def updateAction = task {
    val result = baseUpdateAction.run

    result.orElse{withTemporaryDirectory(log){repository: File =>
        updateCoverage(repository)
        result
      }
    }
  }

  private def updateCoverage(repository: File) {
    log.control(ControlEvent.Start, "== update coverage task ==")
    try {
      checkout(repository)
      installCoverage(new File(repository, "sbt-coverage"))
    }
    finally clean(Path.fromFile(repository), log)

    log.info("Reload sbt to enable coverage tool.")

    log.control(ControlEvent.Finish, "== update coverage task ==")
  }

  private def installCoverage(repository: File){
    ExtermalProcess(sbt ::: List("update"), None, Some(repository.getAbsolutePath), log)
    ExtermalProcess(sbt ::: List("publish-local"), None, Some(repository.getAbsolutePath), log)

    val processorsAndRepos = ExtermalProcess(sbt ::: List("*show"), None, Some(repository.getAbsolutePath), log)
    if (!processorsAndRepos.contains(processorRepo))
      ExtermalProcess(sbt ::: List("*" + processorRepo), None, Some(repository.getAbsolutePath), log)
    if (!processorsAndRepos.contains(processor))
      ExtermalProcess(sbt ::: List("*" + processor), None, Some(repository.getAbsolutePath), log)    
  }

  private def sbt = List("java", "-jar", System.getProperty("user.dir") + "/project/sbt-launch-0.7.5.RC0.jar")

  private def checkout(repository: File) = 
    ExtermalProcess(List("git", "clone", "https://github.com/sroebuck/sbt-coverage.git"), None, Some(repository.getAbsolutePath), log)

  def baseUpdateAction = super.updateAction
}
