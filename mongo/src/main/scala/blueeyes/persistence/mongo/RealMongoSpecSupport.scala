package blueeyes.persistence.mongo

import com.mongodb.{Mongo => TGMongo}

import org.slf4j.LoggerFactory

import org.apache.commons.io.FileUtils
import com.google.common.io.Files

import java.io.File
import java.net.{ServerSocket, URL}

import scalaz.effect.IO

import akka.dispatch.Await
import akka.util.Timeout

import org.specs2.mutable._
import org.specs2.specification.{Fragments, Step}

import scala.sys.process._

import java.util.concurrent.TimeUnit

trait RealMongoSpecSupport extends Specification {
  private[this] var mongoImpl: Mongo = _
  private[this] var realMongoImpl: TGMongo = _

  lazy val mongoLogger = LoggerFactory.getLogger("blueeyes.persistence.mongo.RealMongoSpecSupport")

  def mongo = mongoImpl
  def realMongo = realMongoImpl

  def mongoStartupPause = Some(10000l)

  private var mongoProcess: Option[Process] = None

  def defaultPort = 37017

  private final var workDir: File = Files.createTempDir()
  private final val downloadDir = new File(new File(System.getProperty("java.io.tmpdir")), "blueeyesTestArtifacts")
  downloadDir.mkdirs()
  private final val mongoFile = new File(downloadDir, "mongo-bins.tgz")
  private final val mongoDir = new File(workDir, "mongo")
  private final val dataDir  = new File(workDir, "mongo-data")



  // Shamlessly borrowed from Camel: 
  // http://svn.apache.org/viewvc/camel/trunk/components/camel-test/src/main/java/org/apache/camel/test/AvailablePortFinder.java?view=markup#l130
  private def isAvailable(port: Int): Boolean = {
    var ss: ServerSocket = null
    try {
      var ss = new ServerSocket(port);
      ss.setReuseAddress(true);
      return true;
    } catch {
      case t => // NOOP
    } finally {
      if (ss != null) {
        try {
          ss.close();
        } catch {
          case t => /* should not be thrown */
        }
      }
    }
    return false;
  }


  private def findUnusedPort(startPort: Int): Option[Int] = {
    // Limit to 1000 tries, bail if we can't find a port within that time
    (startPort to (startPort + 1000)).find(isAvailable)
  }

  def startup(): Unit = {
    def mkDirs(d: File) = IO {
      if (! (d.isDirectory || d.mkdirs())) {
        throw new Exception("Failed to make directory: " + d)
      }
    }

    def unpackMongo(downloadFile: File, targetDir: File) = IO {
      if (targetDir.listFiles.length == 0) {
        if (! downloadFile.exists()) {
          mongoLogger.debug("Downloading real mongo dist to " + downloadFile)
          // download and extract the files
          val url = new URL(if (System.getProperty("os.name").startsWith("Mac")) {
            "http://fastdl.mongodb.org/osx/mongodb-osx-x86_64-2.2.0.tgz"
          } else {
            "http://fastdl.mongodb.org/linux/mongodb-linux-x86_64-2.2.0.tgz"
          })
          
          FileUtils.copyURLToFile(url, downloadFile)
        }

        mongoLogger.debug("Unpacking mongo")
        Process("tar", 
                Seq("-C", 
                    targetDir.getCanonicalPath, 
                    "--strip-components", 
                    "1", 
                    "-x", 
                    "-v", 
                    "-z", 
                    "-f", downloadFile.getCanonicalPath)).!!(ProcessLogger(s => mongoLogger.debug(s)))
      }
    }
    
    val io: IO[Unit] = for {
      _ <- mkDirs(downloadDir)
      _ <- mkDirs(mongoDir)
      _ <- mkDirs(dataDir)
      _ <- unpackMongo(mongoFile, mongoDir)
    } yield {
      mongoLogger.debug("Searching for unused port")
      findUnusedPort(defaultPort) match {
        case Some(port) =>
          mongoLogger.info("Starting mongo on port " + port)
          mongoProcess = Some({
            val proc = Process(new File(mongoDir, "bin/mongod").getCanonicalPath, Seq("--port", port.toString, 
                                                                                      "--dbpath", dataDir.getCanonicalPath,
                                                                                      "--nojournal", 
                                                                                      "--nounixsocket", 
                                                                                      "--noauth",
                                                                                      "--noprealloc"))
            proc.run(ProcessLogger(s => mongoLogger.debug(s)))
          })

          mongoStartupPause.foreach { delay =>
            mongoLogger.info("Pausing %d ms for mongo startup".format(delay))
            Thread.sleep(delay)
          }

          // Wait for startup, as there's no way to inquire on process activity
          var startupTries = 10
          while (startupTries > 0 && isAvailable(port)) {
            mongoLogger.debug("Waiting on mongo startup")
            Thread.sleep(1000)
            startupTries -= 1
          }

          if (isAvailable(port)) {
            throw new Exception("Mongo startup failed!")
          }

          realMongoImpl = new TGMongo("localhost", port)
          mongoImpl = new RealMongo(realMongo, Timeout(60, TimeUnit.SECONDS))

          mongoLogger.debug("Mongo started, commencing specs")
        case None => throw new IllegalStateException("Could not find an unused port for test mongo!")
      }
    }

    io.unsafePerformIO
  }

  def shutdown(): Unit = {
    Await.result(mongo.asInstanceOf[RealMongo].close, Timeout(60, TimeUnit.SECONDS).duration)
    mongoProcess.foreach { process =>
      try {
        process.destroy()
      } catch {
        case t: Throwable => // discard
      }
    }
    dataDir.listFiles.foreach(FileUtils.deleteQuietly(_))
  }

  override def map (fs: => Fragments): Fragments = Step { startup() } ^ fs ^ Step { shutdown() }
}
