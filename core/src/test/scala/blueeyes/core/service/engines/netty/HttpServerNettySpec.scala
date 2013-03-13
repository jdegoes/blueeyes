package blueeyes.core.service
package engines.netty

import engines._
import blueeyes._
import blueeyes.util._
import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.test._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.http.HttpStatusCodes._
import engines.security.BlueEyesKeyStoreFactory
import engines.{TestEngineService, HttpClientXLightWeb}
import DefaultBijections._

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import akka.dispatch.Await
import akka.util._

import java.io.File
import java.nio.ByteBuffer

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import blueeyes.akka_testing.FutureMatchers

import org.specs2.mutable.Specification
import org.specs2.specification.{Step, Fragments}
import org.specs2.time.TimeConversions._

import scalaz._
import scalaz.syntax.monad._

class HttpServerNettySpec extends Specification with TestAkkaDefaults with HttpRequestMatchers with FutureMatchers {
  private val configPattern = """server { port = %d sslPort = %d }"""

  val duration: org.specs2.time.Duration = 5000.milliseconds

  private val port = 8585
  private var stop: Option[Stoppable] = None
  private var config: Configuration = null

  override def is = args(sequential = true) ^ super.is

  override def map(fs: => Fragments) = {
    def startStep = Step {
      val conf = Configuration.parse(configPattern.format(port, port + 1), BlockFormat)
      val sampleServer = (new SampleServer).server(conf, defaultFutureDispatch)
      config = sampleServer.config

      sampleServer.start map { startFuture =>
        Await.result(startFuture map { case (_, shutdown) => stop = shutdown }, duration)
      } getOrElse {
        sys.error("No handlers available in service being started; aborting test.")
      }
    } 
    
    def stopStep = Step {
      TestEngineService.dataFile.delete
      stop.foreach(Stoppable.stop(_, duration))
    }

    startStep ^ fs ^ stopStep
  }

  "HttpServer" should {
    "return empty response" in {
      Await.result(client.post("/empty/response")(""), duration) must beLike {
        case HttpResponse(status, _, content, _) =>
          (status.code must be (OK)) and
          (content must beNone)
      }
    }

    "write file" in {
      TestEngineService.dataFile.delete

      //client.post("/file/write")("foo") must whenDelivered {
      Await.result(client.post("/file/write")("foo"), duration) must
        beLike {
          case HttpResponse(status, _, content, _) =>
            (status.code must be (OK)) and
            (TestEngineService.dataFile.exists must be_==(true)) and
            (TestEngineService.dataFile.length mustEqual("foo".length))
        }
      //}
    }

    "read file" in {
      TestEngineService.dataFile.delete

      akka.dispatch.Await.result(client.post("/file/write")("foo"), duration)
      client.get[String]("/file/read") must succeedWithContent {
        be_==("foo")
      }
    }

    "return html by correct URI" in {
      client.get[String]("/bar/foo/adCode.html") must succeedWithContent {
        be_==(TestEngineService.content)
      }
    }

    "return NotFound when accessing a nonexistent URI" in {
      Await.result(client.post("/foo/foo/adCode.html")("foo"), duration) must beLike { 
        case HttpResponse(status, _, content, _) => status.code must_== NotFound
      }
    }

    "return InternalServerError when handling request crashes" in {
      Await.result(client.get("/error"), duration) must beLike { 
        case HttpResponse(status, _, content, _) => status.code must_== InternalServerError
      }
    }

    "return Http error when handling request throws HttpException" in {
      Await.result(client.get("/http/error"), duration) must beLike { 
        case HttpResponse(status, _, content, _) => status.code must_== BadRequest 
      }
    }

    "return html by correct URI with parameters" in {
      client.parameters('bar -> "zar").get[String]("/foo") must succeedWithContent {
        be_==(TestEngineService.content)
      }
    }
    
    "return huge content" in {
      Await.result(client.get[ByteChunk]("/huge"), 10.seconds) must beLike {
        case HttpResponse(status, _, Some(content), _) =>
          (status.code must_== HttpStatusCodes.OK) and
          (readContent(content) must haveSize(TestEngineService.hugeContent.map(v => v.length).sum))
      }
    }

    "return huge delayed content" in {
      implicit val testTimeouts = FutureTimeouts(50, duration)

      Await.result(client.get[ByteChunk]("/huge/delayed"), 10.seconds) must beLike {
        case HttpResponse(status, _, Some(content), _) =>
          (status.code must_== HttpStatusCodes.OK) and
          (readContent(content) must haveSize(TestEngineService.hugeContent.map(v => v.length).sum))
      }
    }

    "return html by correct URI by https" in {
      sslClient.get[String]("/bar/foo/adCode.html") must succeedWithContent {
        be_==(TestEngineService.content)
      }
    }

    "return huge content by https" in {
      Await.result(sslClient.get[ByteChunk]("/huge"), 10.seconds) must beLike {
        case HttpResponse(status, _, Some(content), _) =>
          (status.code must_== HttpStatusCodes.OK) and
          (readContent(content) must haveSize(TestEngineService.hugeContent.map(v => v.length).sum))
      }
    }
  }

  private def readContent(chunk: ByteChunk): Array[Byte] = {
    Await.result(ByteChunk.forceByteArray(chunk), 10.seconds)
  }

  private def readStringContent(chunk: ByteChunk): String = {
    Await.result(ByteChunk.forceByteArray(chunk).map(new String(_, "UTF-8")), 10.seconds)
  }

  private def client    = new LocalClient(config).protocol("http").host("localhost").port(port)

  private def sslClient = new LocalClient(config).protocol("https").host("localhost").port(port + 1)
}

class SampleServer extends BlueEyesServer with TestEngineService with TestAkkaDefaults {
  val executionContext = defaultFutureDispatch
}
