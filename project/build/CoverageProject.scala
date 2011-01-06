import sbt._
import FileUtilities._
import java.io.File

trait CoverageProject extends BasicManagedProject{

  override def updateAction = task {
    withTemporaryDirectory(log){repository: File =>
//      updateCoverage(repository)
      baseUpdateAction.run
    }
  }

  private def updateCoverage(repository: File) {
    
    checkout(repository)
    installCoverage(repository)

    log.info("Restart sbt to enable coverage tool.")
  }

  private def installCoverage(repository: File){
    val sbt = System.getProperty("user.dir") + "/sbt"
    ExtermalProcess(List(sbt, "publish-local"), None, Some(repository.getAbsolutePath), log)
    ExtermalProcess(List(sbt, "*undercoverRepo at http://undercover.googlecode.com/svn/maven/repository/"), None, Some(repository.getAbsolutePath), log)
    ExtermalProcess(List(sbt, "*coverage is com.proinnovate sbt-coverage 0.1-SNAPSHOT"), None, Some(repository.getAbsolutePath), log)    
  }

  private def publish(repository: File) = {
    
  }

  private def checkout(repository: File) = {
    try {
      ExtermalProcess(List("git", "clone", "https://github.com/sroebuck/sbt-coverage.git"), None, Some(repository.getAbsolutePath), log)
    }
    finally clean(Path.fromFile(repository), log)
  }

  def baseUpdateAction = super.updateAction}