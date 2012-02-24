package blueeyes.core.service.test


import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.duration._
import akka.util.DurationLong

import blueeyes.bkka.AkkaDefaults
import blueeyes.core.data.ByteChunk
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}
import blueeyes.core.service._
import blueeyes.util.RichThrowableImplicits._

import java.util.concurrent.{TimeUnit, CountDownLatch}
import net.lag.configgy.{Config, Configgy}

import org.specs2.mutable.Specification
import org.specs2.specification.{Fragment, Fragments, Step}

class BlueEyesServiceSpecification extends Specification with blueeyes.concurrent.test.FutureMatchers with HttpReflectiveServiceList[ByteChunk] with AkkaDefaults { self =>
  private lazy val NotFound    = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound))

  override def is = args(sequential = true) ^ super.is
  private val specBefore = Step {
    setMockConfiguration
    startServer
  }

  private val specAfter = Step {
    resetMockConfiguration
    stopServer
  }

  override def map(fs: =>Fragments) = specBefore ^ Step(beforeSpec _) ^ fs ^ Step(afterSpec _) ^ specAfter

  def startTimeOut   = 60000l
  def stopTimeOut    = 60000l
  def configuration  = ""

  protected def beforeSpec: Any = ()
  protected def afterSpec: Any = ()

  private val httpServer = new HttpServer{
    def services = self.services

    // Manual configuration based on "configuration" string:
    override def rootConfig: Config = self.rootConfig
  }

  val globalKeyState = GlobalKey.box.value

  def setMockConfiguration = {
    GlobalKey.value = Some(true)
  }

  def resetMockConfiguration = {
    GlobalKey.value = globalKeyState
  }

  def service: HttpClient[ByteChunk] = new SpecClient()

  private def startServer = Await.result(httpServer.start, new DurationLong(startTimeOut) millis)
  private def stopServer  = Await.result(httpServer.stop,  new DurationLong(stopTimeOut) millis)

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
        response.toOption.getOrElse(Future(NotFound))
      } catch {
        case t: Throwable => Future(convertErrorToResponse(t))
      }
    }

    def isDefinedAt(x: HttpRequest[ByteChunk]) = true
  }
}
