package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.concurrent.Future
import blueeyes.util.RichThrowableImplicits._
import blueeyes.core.service._
import java.util.concurrent.{TimeUnit, CountDownLatch}
import net.lag.configgy.{Config, Configgy}
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}
import org.specs.specification.{Examples}

class BlueEyesServiceSpecification[T] extends Specification with HttpServer[T] with HttpReflectiveServiceList[T] { self: HttpServer[T] =>
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

  def service: HttpClient[T] = new SpecClient()

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

  private val currentClient = new ThreadLocal[HttpClient[_]]

  implicit def specifyServiceExample(desc: String) = {
    new ServiceExampleSpecification(specifyExample(desc))
  }

  class ServiceExampleSpecification(exampleSpecification: ExampleSpecification){
    def in[S, V](expectations: (HttpClient[S]) => V)(implicit m: scala.reflect.ClassManifest[V]) = {
      val exampleClient = currentClient.get.asInstanceOf[HttpClient[S]]
      exampleSpecification.in[V]({
        expectations(exampleClient)
      })(m)
    }
  }

  implicit def specifyServicesSus[V](httpClient: HttpClient[V]) = {
    new SpecifiedServicesSus(httpClient, specifySus(services.mkString(", ")))
  }

  class SpecifiedServicesSus[V](httpClient: HttpClient[V], specifiedSus: SpecifiedSus){
    def should(noExampleGiven: => Examples): Unit = {
      specifiedSus.should({
        currentClient.set(httpClient)
        noExampleGiven
        currentClient.set(null)
      })
    }
  }

  private class SpecClient extends HttpClient[T]{
    def apply(request: HttpRequest[T]) = {
      def convertErrorToResponse(th: Throwable): HttpResponse[T] = th match {
        case e: HttpException => HttpResponse[T](HttpStatus(e.failure, e.reason))
        case _ => {
          val reason = th.fullStackTrace

          HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
        }
      }

      try {
        self.apply(request)
      }
      catch {
        case t: Throwable => Future[HttpResponse[T]](convertErrorToResponse(t))
      }
    }

    def isDefinedAt(x: HttpRequest[T]) = true
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
