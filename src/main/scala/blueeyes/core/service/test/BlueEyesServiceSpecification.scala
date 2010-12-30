package blueeyes.core.service.test

import org.specs.Specification
import blueeyes.util.Future
import blueeyes.core.http.{HttpRequest}
import org.specs.specification.{Result, Example, ExampleDescription}
import blueeyes.core.service._
import java.util.concurrent.{TimeUnit, CountDownLatch}
import net.lag.configgy.{Config, Configgy}

class BlueEyesServiceSpecification[T] extends Specification with HttpClientTransformerCombinators with HttpServer[T] with HttpReflectiveServiceList[T]{ self: HttpServer[T] =>

  shareVariables()
  doBeforeSpec{startServer}
  doAfterSpec {stopServer}

  def startTimeOut  = 60000
  def stopTimeOut   = 60000
  def configuration = ""

  private def startServer = waitForResponse[Unit](start, startTimeOut)
  private def stopServer  = waitForResponse[Unit](stop, stopTimeOut)

  override def main(args: Array[String]): Unit = super.main(args)

  override def rootConfig: Config = {
    Configgy.configureFromString(configuration)
    Configgy.config
  }  
  
  implicit def resultToFuture[V, S](result: Result[V]) = new Future[S]()

  implicit def specifyExample[S](clientTransformer: HttpClientTransformer[T, S]): SpecifiedExample[S] = new SpecifiedExample[S](clientTransformer)

  private def waitForResponse[V](future: Future[V], timeout: Long) = {
    if (!future.isDelivered){
      val latch        = new CountDownLatch(1)

      future.deliverTo(v => {latch.countDown})

      latch.await(timeout, TimeUnit.MILLISECONDS)
    }
    future.value
  }

  class SpecifiedExample[S](clientTransformer: HttpClientTransformer[T, S]){
    def should(what: String) = {
      val example = forExample

      example.in({
        val countDownLatch = new CountDownLatch(1)  

        val client = new SpecClient(example, what)
        val result = client.exec(clientTransformer)

        result.ifCanceled{ why =>
          countDownLatch.countDown()
          why.foreach(throw _)

        }
        result.deliverTo{ f =>
          countDownLatch.countDown()
        }
      })
    }
  }

  private class SpecClient(example: Example, what: String) extends HttpClient[T]{

    def apply(request: HttpRequest[T]) = {

      example.exampleDescription = ExampleDescription(("HTTP %s %s should " + what).format(request.method.value, request.uri))

      self.apply(request)
    }
  }
}