package blueeyes.persistence.mongo

import com.mongodb.{Mongo => TGMongo}

import org.slf4j.LoggerFactory

import org.apache.commons.io.FileUtils
import com.google.common.io.Files

import java.io.File
import java.net.{InetSocketAddress, ServerSocket, URL}
import java.util.concurrent.TimeUnit

import akka.dispatch.Await
import akka.util.Timeout

import org.specs2.mutable._
import org.specs2.specification.{Fragments, Step}

import scala.sys.process._
import scala.util.Random

import scalaz.effect.IO

trait RealMongoSpecSupport extends Specification {
  private[this] var mongoImpl: Mongo = _
  private[this] var realMongoImpl: TGMongo = _

  lazy val mongoLogger = LoggerFactory.getLogger("blueeyes.persistence.mongo.RealMongoSpecSupport")

  def mongo = mongoImpl
  def realMongo = realMongoImpl

  def mongoStartupPause = Some(10000l)

  private var mongoProcess: Option[Process] = None

  private final var workDir: File = Files.createTempDir()
  private final val downloadDir = new File(new File(System.getProperty("java.io.tmpdir")), "blueeyesTestArtifacts")
  downloadDir.mkdirs()
  private final val mongoFile = new File(downloadDir, "mongo-bins.tgz")
  private final val mongoDir = new File(workDir, "mongo")
  private final val dataDir  = new File(workDir, "mongo-data")

  /** The minimum random port number that can be used by the underlying mongo instance */
  def portRangeStart = 50000
  
  /** The size of the random port range to be used by the underlying mongo instance */
  def portRangeSize  = 10000

  /** The max number of random ports to try */
  def portTries = 1000

  // Shamlessly borrowed from Camel: 
  // http://svn.apache.org/viewvc/camel/trunk/components/camel-test/src/main/java/org/apache/camel/test/AvailablePortFinder.java?view=markup#l130
  private def isAvailable(port: Int): Boolean = {
    var ss: ServerSocket = null
    try {
      ss = new ServerSocket();
      ss.setReuseAddress(true);
      ss.bind(new InetSocketAddress(port))
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

  private def findUnusedPort(tries: Int): Option[Int] = {
    def randomPortStream: Stream[Int] = (Random.nextInt(portRangeSize) + portRangeStart) #:: randomPortStream
    randomPortStream.take(tries).find(isAvailable)
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
      findUnusedPort(portTries) match {
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

          realMongoImpl = new TGMongo("localhost", port)
          mongoImpl = new RealMongo(realMongo, Timeout(60, TimeUnit.SECONDS), None)

          mongoLogger.debug("Mongo started, commencing specs")
        case None => throw new IllegalStateException("Could not find an unused port for test mongo!")
      }
    }

    io.except {
      case t: Throwable => mongoLogger.error("Error during startup", t); throw t
    }.unsafePerformIO
  }

  def shutdown(): Unit = {
    try {
      Await.result(mongo.asInstanceOf[RealMongo].close, Timeout(60, TimeUnit.SECONDS).duration)
    } catch {
      case t => mongoLogger.warn("Error during BlueEyes mongo shutdown", t)
    } finally {
      mongoProcess.foreach { process =>
        try {
          process.destroy()
        } catch {
          case t: Throwable => mongoLogger.warn("Error during RealMongoSpecSupport shutdown", t)
        } finally {
          dataDir.listFiles.foreach(FileUtils.deleteQuietly(_))
        }
      }
    }
  }

  override def map (fs: => Fragments): Fragments = Step { startup() } ^ fs ^ Step { shutdown() }
}
