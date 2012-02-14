package blueeyes.core.service.engines

import blueeyes.core.service._

import akka.dispatch.Future
import akka.dispatch.Promise

import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.http._
import blueeyes.core.data.{FileSink, FileSource, ByteMemoryChunk, ByteChunk, BijectionsChunkString}
import blueeyes.core.http.combinators.HttpRequestCombinators

import java.io.File

trait TestEngineService extends BlueEyesServiceBuilder with HttpRequestCombinators with BijectionsChunkString{
  import blueeyes.core.http.MimeTypes._

  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(TestEngineServiceContext.context))

  val sampleService: Service[ByteChunk] = service("sample", "1.32") { context =>
    request {
      produce(text/html) {
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
            val chunk  = new ByteMemoryChunk(TestEngineServiceContext.hugeContext.head, () => Some(Future(new ByteMemoryChunk(TestEngineServiceContext.hugeContext.tail.head))))

            val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
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
            val promise = Promise[HttpResponse[ByteChunk]]()
            request.content.foreach{value =>
              val f = FileSink(TestEngineServiceContext.dataFile, value)
              f.onSuccess { case v => promise.success(HttpResponse[ByteChunk]()) }
            }
            promise
          }
        } ~
        path("/file/read"){
          get { request: HttpRequest[ByteChunk] =>
            val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = FileSource(TestEngineServiceContext.dataFile))
            Future[HttpResponse[ByteChunk]](response)
          }
        } ~
        path("/huge/delayed"){
          get { request: HttpRequest[ByteChunk] =>

            val promise = Promise[ByteChunk]()
            import scala.actors.Actor.actor
            actor {
              Thread.sleep(2000)
              promise.success(new ByteMemoryChunk(TestEngineServiceContext.hugeContext.tail.head))
            }

            val chunk  = new ByteMemoryChunk(TestEngineServiceContext.hugeContext.head, () => Some(promise))

            val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
            Future[HttpResponse[ByteChunk]](response)
          }
        }
    }
  }
}

object TestEngineServiceContext{
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)

  val hugeContext = List[Array[Byte]]("first-".getBytes ++ Array.fill[Byte](2048*1000)('0'), "second-".getBytes ++ Array.fill[Byte](2048*1000)('0'))
  val context = """<html>
<head>
</head>

<body>
    <h1>Test</h1>
    <h1>Test</h1>
</body>
</html>"""
}
