package blueeyes.core.service.test

import java.util.concurrent.{TimeUnit, CountDownLatch}
import collection.mutable.Stack
import util.DynamicVariable
import blueeyes.util.Future
import blueeyes.core.service._
import blueeyes.core.http.HttpVersions._
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.{HttpStatusCodes, HttpRequest, HttpResponse, HttpStatus, HttpVersion, HttpMethod}
import java.net.InetAddress

trait BlueEyesServiceSpecification[S]  {

  private val _response = new DynamicVariable[Option[HttpResponse[_]]](None)
  private val pathStack: Stack[String] = new Stack[String];

  def path(path: String)(f: => Unit): Unit = {
    pathStack.push(path)

    try { f } finally { pathStack.pop() }
  }

  def get[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(GET, queryParameters, headers, None, remoteHost, version, f, timeout)

  def put[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(PUT, queryParameters, headers, content, remoteHost, version, f, timeout)

  def post[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(POST, queryParameters, headers, content, remoteHost, version, f, timeout)

  def delete[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(DELETE, queryParameters, headers, None, remoteHost, version, f, timeout)

  def options[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(OPTIONS, queryParameters, headers, None, remoteHost, version, f, timeout)

  def head[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(HEAD, queryParameters, headers, None, remoteHost, version, f, timeout)

  def connect[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T], remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(CONNECT, queryParameters, headers, content, remoteHost, version, f, timeout)

  def trace[T](f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), remoteHost: Option[InetAddress] = None,
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(TRACE, queryParameters, headers, None, remoteHost, version, f, timeout)

  def custom[T](method: HttpMethod, queryParameters: Map[Symbol, String], headers: Map[String, String],
             content: Option[T], remoteHost: Option[InetAddress] = None, version: HttpVersion, f: => Unit, timeout: Long = 60000) = {
    val uri         = currentPath
    val handler     = handlerForRequest[T](uri, method)
    val parameters  = handler._1(uri) ++ queryParameters
    val future      = handler._3(HttpRequest[T](method, uri, parameters, headers, content, remoteHost, version))

    withResponse(waitForResponse(future, timeout), f)
  }

  private def waitForResponse[T](future: Future[HttpResponse[T]], timeout: Long) = {
    if (!future.isDelivered){
      val latch        = new CountDownLatch(1)

      future.deliverTo(v => {latch.countDown})

      latch.await(timeout, TimeUnit.MILLISECONDS)
    }
    future.value
  }

  private def handlerForRequest[T](uri: String, method: HttpMethod) = {
    val requestHandler = service.hierarchy.find(handler => handler._1.isDefinedAt(uri) && method == handler._2)
    val value = requestHandler.getOrElse(throw new RuntimeException("Service for request cannot be found. Uri=%s; Method=%s".format(uri, method.toString)))
    value.asInstanceOf[(RestPathPattern, HttpMethod, HttpRequest[T] => Future[HttpResponse[T]], HttpDataTranscoder[T, _])]
  }

  private def withResponse[T](response: Option[HttpResponse[T]], f: => Unit) = _response.withValue(response)(f)

  private def currentPath: String = pathStack.foldRight[String]("") { (element, path) => path + element }

  def response[T]: HttpResponse[T] = _response.value.getOrElse(throw new RuntimeException("Request is not loaded.")).asInstanceOf[HttpResponse[T]]
  def status   = response.status
  def headers  = response.headers
  def content  = response.content
  def version  = response.version

  def service: RestHierarchy[S]
}
