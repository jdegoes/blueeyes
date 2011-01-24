package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.util.Future
import blueeyes.util.RichThrowableImplicits._
import org.specs.specification.{Result, Example, ExampleDescription}
import blueeyes.core.service._
import java.util.concurrent.{TimeUnit, CountDownLatch}
import net.lag.configgy.{Config, Configgy}
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}
import blueeyes.core.service.HttpClientTransformerImplicits

class BlueEyesServiceSpecification[T] extends Specification with HttpClientTransformerCombinators with HttpServer[T] with HttpReflectiveServiceList[T] with HttpClientTransformerImplicits { self: HttpServer[T] =>
  shareVariables()
  
  doBeforeSpec {
    startServer
  }
  
  doAfterSpec {
    stopServer
  }

  def startTimeOut   = 60000
  def stopTimeOut    = 60000
  def requestTimeout = 60000
  def configuration  = ""

  /**
   * {{{
   * val barFuture = serverClient {
   *   path$("/foo") {
   *     get$ { response =>
   *       bar
   *     }
   *   }
   * }
   * }}}
   */
  def serverClient: HttpClient[T] = new HttpClient[T] {
    def apply(r: HttpRequest[T]): Future[HttpResponse[T]] = self.apply(r)
  }

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
  
  implicit def resultToFuture[V](result: Result[V]): Future[Unit] = Future.lift(result).toUnit

  implicit def specifyExample[S](clientTransformer: HttpClientTransformer[T, S]): SpecifiedExample[S] = new SpecifiedExample[S](clientTransformer)

  class SpecifiedExample[S](clientTransformer: HttpClientTransformer[T, S]){
    def should(what: String) = {
      val example = forExample

      example.in({
        val future = new SpecClient(example, what)(clientTransformer)
        waitForResponse(future, Some(1000 * 60 * 5), { why => throw why })
      })
    }
  }

  private class SpecClient(example: Example, what: String) extends HttpClient[T]{
    def apply(request: HttpRequest[T]) = {
      def convertErrorToResponse(th: Throwable): HttpResponse[T] = th match {
        case e: HttpException => HttpResponse[T](HttpStatus(e.failure, e.reason))
        case _ => {
          val reason = th.fullStackTrace

          HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
        }
      }
      
      example.exampleDescription = ExampleDescription(("HTTP %s %s should " + what).format(request.method.value, request.uri))

      try {
        val responseFromServer = self.apply(request)
        
        waitForResponse(responseFromServer, Some(requestTimeout), why => ())
        
        responseFromServer
      }
      catch {
        case t: Throwable => Future[HttpResponse[T]](convertErrorToResponse(t))
      }
    }
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
