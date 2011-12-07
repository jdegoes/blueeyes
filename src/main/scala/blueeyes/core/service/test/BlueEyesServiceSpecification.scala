package blueeyes.core.service.test

import org.specs2.mutable.Specification
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import blueeyes.util.RichThrowableImplicits._
import blueeyes.core.service._
import blueeyes.core.data.{ByteChunk}
import java.util.concurrent.{TimeUnit, CountDownLatch}
import net.lag.configgy.{Config, Configgy}
import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}
import org.specs2.specification.{Fragment, Fragments, Step}

class BlueEyesServiceSpecification extends Specification with blueeyes.concurrent.test.FutureMatchers with HttpReflectiveServiceList[ByteChunk]{ self =>
  private lazy val NotFound    = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound))

  private val mongoSwitch      = sys.props.get(ConfigurableMongo.MongoSwitch)
  private val httpClientSwitch = sys.props.get(ConfigurableHttpClient.HttpClientSwitch)

  override def is = args(sequential = true) ^ super.is
  private val specBefore = Step {
    setMockCongiguration
    startServer
  }

  private val specAfter = Step {
    resetMockCongiguration
    stopServer
  }

  override def map(fs: =>Fragments) = specBefore ^ Step(beforeSpec _) ^ fs ^ Step(afterSpec _) ^ specAfter

  def startTimeOut   = 60000
  def stopTimeOut    = 60000
  def configuration  = ""

  protected def beforeSpec: Any = ()
  protected def afterSpec: Any = ()

  private val httpServer = new HttpServer{
    def services = self.services

    // Manual configuration based on "configuration" string:
    override def rootConfig: Config = self.rootConfig
  }

  def setMockCongiguration = {
    sys.props.getOrElseUpdate (ConfigurableMongo.MongoSwitch, "true")
    sys.props.getOrElseUpdate (ConfigurableHttpClient.HttpClientSwitch, "true")
  }

  def resetMockCongiguration = {
    def setProp(key: String, value: Option[String]) = value match{
      case Some(x) => sys.props.put(key, x)
      case None => sys.props.remove(key)
    }
    setProp(ConfigurableMongo.MongoSwitch, mongoSwitch)
    setProp(ConfigurableHttpClient.HttpClientSwitch, httpClientSwitch)
  }

  def service: HttpClient[ByteChunk] = new SpecClient()

  private def startServer = waitForResponse[Unit](httpServer.start, Some(startTimeOut), why => throw why)
  private def stopServer  = waitForResponse[Unit](httpServer.stop,  Some(stopTimeOut),  why => throw why)

  lazy val rootConfig = {
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
        val response = httpServer.service(request)
        response.toOption.getOrElse(NotFound.future)
      } catch {
        case t: Throwable => Future.sync(convertErrorToResponse(t))
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

      timeout match {
        case Some(timeout) => 
          if (!latch.await(timeout, TimeUnit.MILLISECONDS)) sys.error("Timed out waiting " + timeout + " ms for BlueEyes service startup.")
          
        case None => latch.await()
      }
    }
    
    future.value
  }
}
