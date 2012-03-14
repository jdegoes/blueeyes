package blueeyes.core.service.test


import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.duration._
import akka.util.DurationLong

import blueeyes.bkka.AkkaDefaults
import blueeyes.core.data.ByteChunk
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}
import blueeyes.core.service._
import blueeyes.Environment

//import blueeyes.persistence.mongo.ConfigurableMongo
import blueeyes.util.RichThrowableImplicits._

import java.util.concurrent.{TimeUnit, CountDownLatch}

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import org.specs2.mutable.Specification
import org.specs2.specification.{Fragment, Fragments, Step}

class BlueEyesServiceSpecification extends Specification with blueeyes.concurrent.test.FutureMatchers with HttpReflectiveServiceList[ByteChunk] with AkkaDefaults { self =>
  private lazy val NotFound    = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound))

  private val mockSwitch = sys.props.get(Environment.MockSwitch)

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
  def httpServerStopTimeout = stopTimeOut
  def configuration  = ""

  protected def beforeSpec: Any = ()
  protected def afterSpec: Any = ()

  private val httpServer = new HttpServer{
    // For the purposes of tests, kill the server early since we don't care about losing data in a spec
    override val stopTimeout = akka.util.Timeout(httpServerStopTimeout - 5000)

    def services = self.services

    override def rootConfig: Configuration = self.rootConfig
  }

  def setMockCongiguration = {
    sys.props.getOrElseUpdate (Environment.MockSwitch, "true")
  }

  def resetMockCongiguration = {
    def setProp(key: String, value: Option[String]) = value match{
      case Some(x) => sys.props.put(key, x)
      case None => sys.props.remove(key)
    }
    setProp(Environment.MockSwitch, mockSwitch)
  }

  def service: HttpClient[ByteChunk] = new SpecClient()

  private def startServer = Await.result(httpServer.start, new DurationLong(startTimeOut) millis)
  private def stopServer  = Await.result(httpServer.stop,  new DurationLong(stopTimeOut) millis)

  lazy val rootConfig = Configuration.parse(configuration, BlockFormat)

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
        response.toOption.getOrElse(Future(NotFound))
      } catch {
        case t: Throwable => Future(convertErrorToResponse(t))
      }
    }

    def isDefinedAt(x: HttpRequest[ByteChunk]) = true
  }
}
