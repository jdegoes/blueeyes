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
  doBeforeSpec{startServer}
  doAfterSpec {stopServer}

  def startTimeOut  = 60000
  def stopTimeOut   = 60000
  def configuration = ""

  private def startServer = waitForResponse[Unit](start, Some(startTimeOut), {why => throw why})
  private def stopServer  = waitForResponse[Unit](stop, Some(stopTimeOut),   {why => throw why})

  override def main(args: Array[String]): Unit = super.main(args)

  override def rootConfig: Config = {
    Configgy.configureFromString(configuration)
    Configgy.config
  }  
  
  implicit def resultToFuture[V, S](result: Result[V]) = Future.dead[S]

  implicit def specifyExample[S](clientTransformer: HttpClientTransformer[T, S]): SpecifiedExample[S] = new SpecifiedExample[S](clientTransformer)

  private def waitForResponse[V](future: Future[V], timeout: Option[Long], f: Throwable => Unit) = {
    if (!future.isDelivered){
      val latch        = new CountDownLatch(1)

      future.deliverTo(v => {latch.countDown})
      future.ifCanceled{ why =>
        latch.countDown()
        why.foreach(f(_))
      }      

      timeout.map(latch.await(_, TimeUnit.MILLISECONDS)).getOrElse(latch.await())
    }
    future.value
  }

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
    def convertErrorToResponse(th: Throwable): HttpResponse[T] = th match {
      case e: HttpException => HttpResponse[T](HttpStatus(e.failure, e.reason))
      case _ => {
        val reason = th.fullStackTrace
        
        HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
      }
    }
    
    def apply(request: HttpRequest[T]) = {
      example.exampleDescription = ExampleDescription(("HTTP %s %s should " + what).format(request.method.value, request.uri))

      try {
        self.apply(request)
      }
      catch {
        case t: Throwable => Future[HttpResponse[T]](convertErrorToResponse(t))
      }
    }
  }
}
