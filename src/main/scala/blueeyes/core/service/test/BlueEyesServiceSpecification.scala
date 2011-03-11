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
import org.specs.util.Duration
import org.specs.util.TimeConversions._

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

  def eventually(what: String): Eventually = eventually(40, 1000.milliseconds)(what)

  def eventually(retries: Int, sleep: Duration)(what: String): Eventually = Eventually(retries, sleep, "eventually " + what)

  implicit def resultToFuture[V](result: Result[V]): Future[Unit] = Future.lift(result).toUnit

  implicit def specifyExample[S](clientTransformer: HttpClientTransformer[T, S]): SpecifiedExample[S] = new SpecifiedExample[S](clientTransformer)

  class SpecifiedExample[S](clientTransformer: HttpClientTransformer[T, S]){

    def should(eventually: Eventually) = runExample(eventually.retries, eventually.sleep)("should " + eventually.what)

    def should(what: String) = should(Eventually(40, 1000.milliseconds, what))

    private def runExample(retries: Int, sleep: Duration)(what: String){
      val example = forExample

      example.in({

        def retry(future: Future[S], retries: Int, sleep: Duration): Unit = {
          waitForResponse(future, Some(sleep.inMillis), { why => throw why })

          if (!future.isDelivered && retries > 1){
            retry(future, retries - 1, sleep)
          }
        }

        val future = new SpecClient(example, what)(clientTransformer)

        retry(future, retries, sleep)
      })
    }
  }

  case class Eventually(retries: Int, sleep: Duration, what: String)

  private class SpecClient(example: Example, what: String) extends HttpClient[T]{
    def apply(request: HttpRequest[T]) = {
      def convertErrorToResponse(th: Throwable): HttpResponse[T] = th match {
        case e: HttpException => HttpResponse[T](HttpStatus(e.failure, e.reason))
        case _ => {
          val reason = th.fullStackTrace

          HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
        }
      }
      
      example.exampleDescription = ExampleDescription(("HTTP %s %s " + what).format(request.method.value, request.uri))

      try {
        self.apply(request)
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
