package blueeyes.core.service.test

import java.util.concurrent.{TimeUnit, CountDownLatch}
import collection.mutable.Stack
import util.DynamicVariable
import blueeyes.util.Future
import blueeyes.core.service._
import blueeyes.core.http.HttpVersions._
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.{HttpRequest, HttpResponse, HttpVersion, HttpMethod}
import java.net.InetAddress
import org.spex.Specification
import net.lag.configgy.{Config, Configgy}

trait BlueEyesServiceSpecification[T] extends Specification with HttpServer[T] with HttpReflectiveServiceList[T] {  self: Specification =>

  private val _response = new DynamicVariable[Option[HttpResponse[T]]](None)
  private val pathStack: Stack[String] = new Stack[String];

  shareVariables()  
  doBeforeSpec{startServer}
  doAfterSpec {stopServer}

  def startTimeOut  = 60000
  def stopTimeOut   = 60000
  def configuration = ""

  private def startServer = waitForResponse[Unit](start, startTimeOut)
  private def stopServer  = waitForResponse[Unit](stop, stopTimeOut)

  def path(path: String)(f: => Unit): Unit = {
    pathStack.push(path)

    try { f } finally { pathStack.pop() }
  }

  override def rootConfig: Config = {
    Configgy.configureFromString(configuration)
    Configgy.config
  }

  override def main(args: Array[String]): Unit = super.main(args)

  def get(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(GET, queryParameters, headers, None, remoteHost, version, f, timeout)

  def put(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(PUT, queryParameters, headers, content, remoteHost, version, f, timeout)

  def post(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(POST, queryParameters, headers, content, remoteHost, version, f, timeout)

  def delete(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(DELETE, queryParameters, headers, None, remoteHost, version, f, timeout)

  def options(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(OPTIONS, queryParameters, headers, None, remoteHost, version, f, timeout)

  def head(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(HEAD, queryParameters, headers, None, remoteHost, version, f, timeout)

  def connect(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(CONNECT, queryParameters, headers, content, remoteHost, version, f, timeout)

  def trace(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(TRACE, queryParameters, headers, None, remoteHost, version, f, timeout)

  def custom(method: HttpMethod, queryParameters: Map[Symbol, String], headers: Map[String, String],
             content: Option[T], remoteHost: Option[InetAddress] = None, version: HttpVersion, f: => Unit, timeout: Long = 60000) = {
    val request = HttpRequest[T](method, currentPath, queryParameters, headers, content, remoteHost, version)
    val future  = apply(request)

    withResponse(waitForResponse[HttpResponse[T]](future, timeout), f)
  }

  private def waitForResponse[V](future: Future[V], timeout: Long) = {
    if (!future.isDelivered){
      val latch        = new CountDownLatch(1)

      future.deliverTo(v => {latch.countDown})

      latch.await(timeout, TimeUnit.MILLISECONDS)
    }
    future.value
  }
  
  private def withResponse(response: Option[HttpResponse[T]], f: => Unit) = _response.withValue(response)(f)

  private def currentPath: String = pathStack.foldRight[String]("") { (element, path) => path + element }

  def response: HttpResponse[T] = _response.value.getOrElse(throw new RuntimeException("Request is not loaded."))
  def responseStatus   = response.status
  def responseHeaders  = response.headers
  def responseContent  = response.content
  def responseVersion  = response.version
  
//  def config: String
}
