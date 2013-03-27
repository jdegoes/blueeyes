package blueeyes.core.service.test

import akka.dispatch.Future
import akka.dispatch.Await
import akka.dispatch.ExecutionContext
import akka.util.duration._
import akka.util.DurationLong

import blueeyes.bkka._
import blueeyes.core.data.ByteChunk
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpStatus, HttpStatusCodes, HttpException}
import blueeyes.core.service._
import blueeyes.Environment
import blueeyes.util.RichThrowableImplicits._

import com.weiglewilczek.slf4s.Logging

import java.util.concurrent.{TimeUnit, CountDownLatch}

import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

import org.specs2.mutable.Specification
import org.specs2.specification.{Fragment, Fragments, Step}

import blueeyes.akka_testing.FutureMatchers

abstract class BlueEyesServiceSpecification extends Specification with FutureMatchers with HttpServerModule with ReflectiveServiceList[ByteChunk] with Logging { self =>
  private val NotFound = HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.NotFound))
  private val mockSwitch = sys.props.get(Environment.MockSwitch)

  private var _service: AsyncHttpService[ByteChunk] = _
  private var _stoppable: Option[Stoppable] = None

  private val specBefore = Step {
    setMockCongiguration

    val config = Configuration.parse(configuration, BlockFormat)
    server(config, executionContext).start map { startFuture =>
      val started = startFuture map { 
        case (service, stoppable) => 
          _service = service
          _stoppable = stoppable
      } onFailure {
        case ex => 
          System.err.println("Unable to begin test due to error in service start.")
          ex.printStackTrace(System.err)
      }

      Await.result(started, new DurationLong(startTimeOut) millis)
    }
  }

  private val specAfter = Step {
    resetMockCongiguration
    stoppable.foreach(Stoppable.stop(_, new DurationLong(stopTimeOut) millis))
  }

  override def is = args(sequential = true) ^ super.is
  override def map(fs: => Fragments) = specBefore ^ beforeSpec ^ fs ^ afterSpec ^ specAfter

  def startTimeOut   = 60000
  def stopTimeOut    = 60000
  def httpServerStopTimeout = stopTimeOut
  def configuration  = ""
  implicit def executionContext: ExecutionContext

  protected def beforeSpec: Step = Step()
  protected def afterSpec: Step = Step()

  final def service = _service
  final def stoppable = _stoppable

  class HttpServer(config: Configuration, executor: ExecutionContext) extends HttpServerLike(config) {
    override implicit val executionContext = executor

    // For the purposes of tests, kill the server early since we don't care about losing data in a spec
    override val stopTimeout = akka.util.Timeout(httpServerStopTimeout - 5000)

    override val services = self.services
    override val log = self.logger
  }

  def server(config: Configuration, executor: ExecutionContext) = new HttpServer(config, executor)

  def setMockCongiguration = {
    sys.props.getOrElseUpdate (Environment.MockSwitch, "true")
  }

  def resetMockCongiguration = {
    def setProp(key: String, value: Option[String]) = value match {
      case Some(x) => sys.props.put(key, x)
      case None => sys.props.remove(key)
    }

    setProp(Environment.MockSwitch, mockSwitch)
  }

  object client extends HttpClient[ByteChunk] {
    def isDefinedAt(x: HttpRequest[ByteChunk]) = true

    def apply(request: HttpRequest[ByteChunk]) = {
      def convertErrorToResponse(th: Throwable): HttpResponse[ByteChunk] = th match {
        case e: HttpException => HttpResponse[ByteChunk](HttpStatus(e.failure, e.reason))
        case _ => {
          val reason = th.fullStackTrace

          HttpResponse[ByteChunk](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
        }
      }

      try {
        val response = service.service(request)
        response.toOption.getOrElse(Future(NotFound))
      } catch {
        case t: Throwable => 
          logger.error("Error reported in service.", t)
          Future(convertErrorToResponse(t))
      }
    }
  }
}
