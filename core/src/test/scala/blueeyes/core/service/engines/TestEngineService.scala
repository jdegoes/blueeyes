package blueeyes.core.service.engines

import blueeyes.bkka._
import blueeyes.core.service._
import security._

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import blueeyes.BlueEyesServiceBuilder
import blueeyes.core.data._
import blueeyes.core.http._
import blueeyes.core.http.combinators.HttpRequestCombinators
import blueeyes.core.service._
import DefaultBijections._

import org.streum.configrity.Configuration

import java.io.File
import java.nio.ByteBuffer
import javax.net.ssl.TrustManagerFactory

import scalaz.StreamT
import scalaz.syntax.monad._

trait TestEngineService extends BlueEyesServiceBuilder with HttpRequestCombinators with TestAkkaDefaults {
  import blueeyes.core.http.MimeTypes._
  import HttpRequestHandlerImplicits._

  private val response = HttpResponse[String](status = HttpStatus(HttpStatusCodes.OK), content = Some(TestEngineService.content))

  val sampleService = service("sample", "1.32") { _ =>
    request {
      encode[ByteChunk, Future[HttpResponse[String]], Future[HttpResponse[ByteChunk]]] {
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
        }
      } ~
      path("/huge"){
        get { request: HttpRequest[ByteChunk] =>
          val chunk = Right(StreamT.fromStream[Future, ByteBuffer](Future(TestEngineService.hugeContent.toStream.map(ByteBuffer.wrap _))))
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
            val (_, written) = FileSink.write(TestEngineService.dataFile, value) 
            written map { _ => HttpResponse[ByteChunk]() }
          } getOrElse {
            Promise.successful(HttpResponse[ByteChunk]())
          }
        }
      } ~
      path("/file/read"){
        get { request: HttpRequest[ByteChunk] =>
          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(new FileSource(TestEngineService.dataFile).read))
          Future[HttpResponse[ByteChunk]](response)
        }
      } ~
      path("/huge/delayed"){
        get { request: HttpRequest[ByteChunk] =>

          val promise = Promise[ByteBuffer]()
          import scala.actors.Actor.actor
          actor {
            Thread.sleep(2000)
            promise.success(ByteBuffer.wrap(TestEngineService.hugeContent.tail.head))
          }

          val chunk = Right(ByteBuffer.wrap(TestEngineService.hugeContent.head) :: (promise: Future[ByteBuffer]).liftM[StreamT])
          val response     = HttpResponse[ByteChunk](status = HttpStatus(HttpStatusCodes.OK), content = Some(chunk))
          Future[HttpResponse[ByteChunk]](response)
        }
      }
    }
  }
}

object TestEngineService {
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)

  val first: Array[Byte] = Array.concat("first-".getBytes("UTF-8"), Array.fill[Byte](2048*1000)('0'))
  val second: Array[Byte] = Array.concat("second-".getBytes("UTF-8"), Array.fill[Byte](2048*1000)('0'))
  val hugeContent = List[Array[Byte]](first, second)

  val content = """<html>
<head></head>
<body>
    <h1>Test</h1>
    <h1>Test</h1>
</body>
</html>"""
}

class LocalClient(config: Configuration)(implicit executor: ExecutionContext) extends HttpClientXLightWeb {
  override protected def createSSLContext = {
    val keyStore            = BlueEyesKeyStoreFactory(config)
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    trustManagerFactory.init(keyStore)

    SslContextFactory(keyStore, BlueEyesKeyStoreFactory.password, Some(trustManagerFactory.getTrustManagers))
  }
}
