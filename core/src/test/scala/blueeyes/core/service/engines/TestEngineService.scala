package blueeyes.core.service.engines

import blueeyes.bkka._
import blueeyes.core.service._

import akka.dispatch.Future
import akka.dispatch.Promise

import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.service._
import DefaultBijections._

import java.io.File
import java.nio.ByteBuffer

import scalaz.StreamT
import scalaz.syntax.monad._

trait TestEngineService extends BlueEyesServiceBuilder with HttpRequestCombinators with TestAkkaDefaults {
  import blueeyes.core.http.MimeTypes._

  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(TestEngineServiceContext.context))

  val sampleService = service("sample", "1.32") { context =>
    request {
      produce[ByteChunk, String, ByteChunk](text/html) {
        path("/bar/'adId/adCode.html") {
          get { request: HttpRequest[ByteChunk] =>
            Future[HttpResponse[String]](response)
          }
        } ~
        path("/foo") {
          get { request: HttpRequest[ByteChunk] =>
            Future[HttpResponse[String]](response)
          }
        } ~
        path("/error") {
          get[ByteChunk, Future[HttpResponse[String]]] { request: HttpRequest[ByteChunk] =>
            throw new RuntimeException("Unexpected error (GET /error)")
          }
        } ~
        path("/http/error") {
          get[ByteChunk, Future[HttpResponse[String]]] { request: HttpRequest[ByteChunk] =>
            throw HttpException(HttpStatusCodes.BadRequest)
          }
        }
      } ~
      path("/huge"){
        get { request: HttpRequest[ByteChunk] =>
          val chunk = Right(StreamT.fromStream[Future, ByteBuffer](Future(TestEngineServiceContext.hugeContent.toStream.map(ByteBuffer.wrap _))))
          val response = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/empty/response"){
        post { request: HttpRequest[ByteChunk] =>
          Future[HttpResponse[ByteChunk]](HttpResponse[ByteChunk]())
        }
      } ~
      path("/file/write"){
        post { request: HttpRequest[ByteChunk] =>
          request.content map { value =>
            FileSink.write(TestEngineServiceContext.dataFile, value) map {
              _ => HttpResponse[ByteChunk]()
            }
          } getOrElse {
            Promise.successful(HttpResponse[ByteChunk]())
          }
        }
      } ~
      path("/file/read"){
        get { request: HttpRequest[ByteChunk] =>
          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(new FileSource(TestEngineServiceContext.dataFile).read))
          Future[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/huge/delayed"){
        get { request: HttpRequest[ByteChunk] =>

          val promise = Promise[ByteBuffer]()
          import scala.actors.Actor.actor
          actor {
            Thread.sleep(2000)
            promise.success(ByteBuffer.wrap(TestEngineServiceContext.hugeContent.tail.head))
          }

          val chunk = Right(ByteBuffer.wrap(TestEngineServiceContext.hugeContent.head) :: (promise: Future[ByteBuffer]).liftM[StreamT])
          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future[HttpResponse[ByteChunk]](response)
        }
      }
    }
  }
}

object TestEngineServiceContext {
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)

  val first: Array[Byte] = Array.concat("first-".getBytes("UTF-8"), Array.fill[Byte](2048*1000)('0'))
  val second: Array[Byte] = Array.concat("second-".getBytes("UTF-8"), Array.fill[Byte](2048*1000)('0'))
  val hugeContent = List[Array[Byte]](first, second)

  val context = """<html>
<head></head>
<body>
    <h1>Test</h1>
    <h1>Test</h1>
</body>
</html>"""
}
