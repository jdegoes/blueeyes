package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.concurrent.Future
import blueeyes.util.RichThrowableImplicits._
import blueeyes.core.service._
import blueeyes.core.data.{BijectionsIdentity, ByteChunk}
import java.util.concurrent.{TimeUnit, CountDownLatch}
import net.lag.configgy.{Config, Configgy}
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}

class BlueEyesServiceSpecification extends Specification with HttpServer with HttpReflectiveServiceList[ByteChunk] with BijectionsIdentity{ self: HttpServer =>
  shareVariables()

  doBeforeSpec {
    startServer
  }

  doAfterSpec {
    stopServer
  }

  def startTimeOut   = 60000
  def stopTimeOut    = 60000
  def configuration  = ""

  def service: HttpClient[ByteChunk] = new SpecClient()

  private def startServer = waitForResponse[Unit](start, Some(startTimeOut), why => throw why)
  private def stopServer  = waitForResponse[Unit](stop,  Some(stopTimeOut),  why => throw why)

  // Revert main function of HttpServer to Specification's main function:
  override def main(args: Array[String]): Unit = super.main(args)

  // Manual configuration based on "configuration" string:
  override def rootConfig: Config = _rootConfig
  
  private lazy val _rootConfig = {
    Configgy.configureFromString(configuration)
    Configgy.config
  }

  private class SpecClient extends HttpClient[ByteChunk]{
    def apply(request: HttpRequest[ByteChunk]) = {
      def convertErrorToResponse(th: Throwable): HttpResponse[ByteChunk] = th match {
        case e: HttpException => HttpResponse[ByteChunk](HttpStatus(e.failure, e.reason))
        case _ => {
          val reason = th.fullStackTrace

          HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
        }
      }

      try {
        self.apply(request)
      }
      catch {
        case t: Throwable => Future[HttpResponse[ByteChunk]](convertErrorToResponse(t))
      }
    }

    def isDefinedAt(x: HttpRequest[ByteChunk]) = true
  }

  private def waitForResponse[S](future: Future[S], timeout: Option[Long], f: Throwable => Unit): Option[S] = {
    if (!future.isDelivered) {
      val latch = new CountDownLatch(1)

      future.deliverTo(v => latch.countDown)
      future.ifCanceled{ why =>
        latch.countDown()
        why.foreach(f(_))
      }      

      timeout.map(latch.await(_, TimeUnit.MILLISECONDS)).getOrElse(latch.await())
    }
    
    future.value
  }
}
