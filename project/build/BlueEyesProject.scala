import sbt._
import de.element34.sbteclipsify._

trait OneJar { this: DefaultProject =>
  lazy val oneJar = oneJarAction

  def oneJarAction = oneJarTask.dependsOn(`package`) describedAs("Creates a single JAR containing all dependencies that runs the project's mainClass")

  def oneJarTask: Task = task {
    import FileUtilities._
    import java.io.{ByteArrayInputStream, File}
    import java.util.jar.Manifest
    import org.apache.commons.io.FileUtils

    val manifest = new Manifest(new ByteArrayInputStream((
      "Manifest-Version: 1.0\n" +
      "Main-Class: " + mainClass.get + "\n").getBytes))

    val versionString = version match {
      case BasicVersion(major, _, _, _) => "-v" + major.toString

      case _ => version.toString
    }

    val allDependencies = jarPath +++ runClasspath +++ mainDependencies.scalaJars

    log.info("All dependencies of " + name + ": " + allDependencies)

    val destJar = (normalizedName + versionString + ".jar"): Path

    FileUtilities.withTemporaryDirectory(log) { tmpDir =>
      val tmpPath = Path.fromFile(tmpDir)

      allDependencies.get.foreach { dependency =>
        log.info("Unzipping " + dependency + " to " + tmpPath)

        if (dependency.ext.toLowerCase == "jar") {
          unzip(dependency, tmpPath, log)
        }
        else if (dependency.asFile.isDirectory) {
          FileUtils.copyDirectory(dependency.asFile, tmpDir)
        }
        else {
          copyFile(dependency.asFile, tmpDir, log)
        }
      }

      new File(tmpDir, "META-INF/MANIFEST.MF").delete

      log.info("Creating single jar out of all dependencies: " + destJar)

      jar(tmpDir.listFiles.map(Path.fromFile), destJar, manifest, true, log)

      None
    }
  }
}

class BlueEyesProject(info: ProjectInfo) extends DefaultProject(info) with Repositories with Eclipsify with IdeaProject with PublishingProject with GpgPlugin with ChecksumPlugin with CoverageProject{
  val commons_io    = "commons-io"                  %  "commons-io"         % "2.0.1"         % "compile"
  val specs         = "org.scala-tools.testing"     %% "specs"              % "1.6.7"         % "compile"
  val scala_check   = "org.scala-tools.testing"     %% "scalacheck"         % "1.8"           % "compile"
  val scala_test    = "org.scalatest"               % "scalatest"           % "1.3"           % "test"
  val mockito       = "org.mockito"                 % "mockito-all"         % "1.8.5"         % "compile"
  val paranamer     = "com.thoughtworks.paranamer"  % "paranamer"           % "2.3"
  val junit         = "junit"                       % "junit"               % "4.8.2"         % "compile"
  val netty         = "org.jboss.netty"             % "netty"               % "3.2.4.Final"   % "compile"
  val mongo         = "org.mongodb"                 % "mongo-java-driver"   % "2.5.2"         % "compile"
  val joda_time     = "joda-time"                   % "joda-time"           % "1.6.2"         % "compile"
  val configgy      = "net.lag"                     % "configgy"            % "2.0.0"         % "compile" intransitive()
  val guice         = "com.google.inject"           % "guice"               % "2.0"           % "compile"
  val rhino         = "rhino"                       % "js"                  % "1.7R2"         % "compile"
  val xlightweb     = "org.xlightweb"               % "xlightweb"           % "2.13.2"        % "compile"
  val codec         = "commons-codec"               % "commons-codec"       % "1.5"           % "compile"
  val clhm_lru      = "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1" % "compile"
  val collections   = "commons-collections"         % "commons-collections" % "3.2.1"         % "compile"
  val scalaz_core   = "org.scalaz"                  %% "scalaz-core"        % "6.0-SNAPSHOT"

  lazy val benchmark = benchmarkTask

  def benchmarkTask = task { args =>
    val duration = if (args.isEmpty) "600" else args(0)
    runTask(Some("blueeyes.benchmark.Benchmark"), runClasspath, Array(duration)) dependsOn(compile, copyResources) describedAs("Run benchmark test")
  }

  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")

  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

  override def mainClass = Some("blueeyes.demo.BlueEyesDemo")
}

trait Repositories {
  val scalareleases   = MavenRepository("Scala Repo Releases",        "http://scala-tools.org/repo-releases/")
  val scalasnapshots  = MavenRepository("Scala-tools.org Repository", "http://scala-tools.org/repo-snapshots/")
  val jbossreleases   = MavenRepository("JBoss Releases",             "http://repository.jboss.org/nexus/content/groups/public/")
  val sonatyperelease = MavenRepository("Sonatype Releases",          "http://oss.sonatype.org/content/repositories/releases")
  val nexusscalatools = MavenRepository("Nexus Scala Tools",          "http://nexus.scala-tools.org/content/repositories/releases")
  val mavenrepo1      = MavenRepository("Maven Repo 1",               "http://repo1.maven.org/maven2/")
  val scalablerepo    = MavenRepository("Maven Repo 1",               "http://scalablesolutions.se/akka/repository")
}
