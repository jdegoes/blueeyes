package blueeyes.core.service
package engines.servlet

import engines.{HttpClientXLightWeb, TestEngineService}

import blueeyes._
import blueeyes.bkka._
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.test._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.MimeTypes._
import DefaultBijections._

import akka.dispatch.{Await, Promise}
import akka.util.Duration

import java.util.concurrent.TimeUnit

import org.specs2.specification.{Step, Fragments}
import org.specs2.mutable.Specification
import org.specs2.time.TimeConversions._

class HttpServerServletSpec extends Specification with TestAkkaDefaults with HttpRequestMatchers {
  val duration = Duration(1000, "millis")
  val retries = 50
  implicit val futureTimeouts: FutureTimeouts = FutureTimeouts(10, Duration(1000l, TimeUnit.MILLISECONDS))

  private var port= 8585
  private var server: Option[JettyServer] = None

  /*
  override def is = args(sequential = true) ^ super.is
  override def map(fs: => Fragments) = {
    val startStep = Step {
        val server = new JettyServer(new ServletTestEngineService())
        server.start(port)

        this.server = Some(server)
    }

    val stopStep = Step {
      TestEngineService.dataFile.delete
      server.foreach(_.stop)
    }

    startStep ^ fs ^ stopStep
  } 
  */

  "HttpServer" should {
    "return empty response" in todo /*{
      Await.result(client.post("/empty/response")(""), duration) must beLike {
        case HttpResponse(status, _, content, _) =>
          (status.code must be (OK)) and
          (content must beNone)
      }
    }*/

    "write file" in todo /*{
      TestEngineService.dataFile.delete

      Await.result(client.post("/file/write")("foo"), duration) must beLike {
        case HttpResponse(status, _, content, _) =>
          (status.code must be (OK)) and
          (TestEngineService.dataFile.exists must be_==(true)) and
          (TestEngineService.dataFile.length mustEqual("foo".length))
      }
    }*/

    "read file" in todo /*{
      TestEngineService.dataFile.delete

      Await.result(client.post("/file/write")("foo") flatMap { _ => client.get[String]("/file/read") }, duration) must beLike {
        case HttpResponse(status, _, content, _) =>
          (status.code must be (OK)) and
          (content must beSome("foo"))
      }
    }*/

    "return html by correct URI" in todo /*{
      Await.result(client.get("/bar/foo/adCode.html"), duration) must beLike {
        case HttpResponse(status, _, content, _) =>
          (status.code must be (OK)) and
          (content must beSome(TestEngineService.content))
      }
    }*/

    "return NotFound when accessing a nonexistent URI" in todo /*{
      val response = Await.result(client.get("/foo/foo/adCode.html").failed, duration)
      response must beLike { case HttpException(failure, _) => failure must_== NotFound }
    }*/

    "return InternalServerError when handling request crashes" in todo /*{
      val response = Await.result(client.get("/error").failed, duration)
      response must beLike { case HttpException(failure, _) => failure must_== InternalServerError }
    }*/

    "return Http error when handling request throws HttpException" in todo /*{
      val response = Await.result(client.get("/http/error").failed, duration)
      response must beLike { case HttpException(failure, _) => failure must_== BadRequest }
    }*/

    "return html by correct URI with parameters" in todo /*{
      Await.result(client.parameters('bar -> "zar").get("/foo"), duration) must beLike {
        case HttpResponse(status, _, content, _) =>
          (status.code must be (OK)) and
          (content must beSome(TestEngineService.content))
      }
    }*/

    "return huge content" in todo /*{
      val result = client.get[ByteChunk]("/huge") 
      result must succeedWithContent {
        (data: ByteChunk) => ByteChunk.forceByteArray(data) must whenDelivered {
          (arr: Array[Byte]) => new String(arr, "UTF-8") must_== TestEngineService.hugeContent.map(v => new String(v).mkString("")).mkString("")
        }
      }
    }*/

    "return huge delayed content" in todo /*{
      val result = client.get[ByteChunk]("/huge/delayed") 
      result must succeedWithContent {
        (data: ByteChunk) => ByteChunk.forceByteArray(data) must whenDelivered {
          (arr: Array[Byte]) => new String(arr, "UTF-8") must_== TestEngineService.hugeContent.map(v => new String(v).mkString("")).mkString("")
        }
      }
    }*/
  }

  private def client = new HttpClientXLightWeb().protocol("http").host("localhost").port(port)
}

class ServletTestEngineService extends ServletServer with TestEngineService 
