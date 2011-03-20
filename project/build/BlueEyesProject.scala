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
  val commonsio      = "commons-io"                   % "commons-io"          % "1.3.2"         % "test"
  val scalatest      = "org.scalatest"                % "scalatest"           % "1.2"           % "test"
  val scalacheck     = "org.scala-tools.testing"      % "scalacheck_2.8.0"    % "1.7"           % "compile"
  val mockito        = "org.mockito"                  % "mockito-all"         % "1.8.4"         % "compile"
  val paranamer      = "com.thoughtworks.paranamer"   % "paranamer"           % "2.0"
  val junit          = "junit"                        % "junit"               % "4.7"           % "compile"
  val scalaspec      = "org.scala-tools.testing"      % "specs_2.8.0"         % "1.6.6-SNAPSHOT"       % "compile"  
  val netty          = "org.jboss.netty"              % "netty"               % "3.2.3.Final"   % "compile"
  val async          = "com.ning"                     % "async-http-client"   % "1.3.3"         % "compile"
  val mongo          = "org.mongodb"                  % "mongo-java-driver"   % "2.5"           % "compile"
  val jodatime       = "joda-time"                    % "joda-time"           % "1.6.2"         % "compile"
  val configgy       = "net.lag"                      % "configgy"            % "2.0.0"         % "compile" intransitive()
  val guice          = "com.google.inject"            % "guice"               % "2.0"           % "compile"
  val rhino          = "rhino"                        % "js"                  % "1.7R2"         % "compile"
  val xlightweb      = "org.xlightweb"                % "xlightweb"           % "2.13.2"        % "compile"
  val commonscodec   = "commons-codec"                % "commons-codec"       % "1.4"           % "compile"
  val clhm_lru       = "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.1" % "compile"
  val apacheColl     = "commons-collections"          % "commons-collections" % "3.2.1"         % "compile"
  val scalazCore     = "org.scalaz"                   %% "scalaz-core"        % "6.0-SNAPSHOT"
  
  lazy val benchmark = benchmarkTask

  def benchmarkTask = task { args =>
    val duration = if (args.isEmpty) "600" else args(0)
    runTask(Some("blueeyes.benchmark.Benchmark"), runClasspath, Array(duration)) dependsOn(compile, copyResources) describedAs("Run benchmark test") 
  }

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