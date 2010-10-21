package blueeyes.core.service.test

import collection.mutable.Stack
import util.DynamicVariable
import blueeyes.core.service._
import blueeyes.core.service.HttpVersions._
import blueeyes.core.service.HttpMethods._
import blueeyes.util.Future
import java.util.concurrent.{TimeUnit, CountDownLatch}

trait BlueEyesServiceSpecification[T]  {

  private val _response = new DynamicVariable[Option[HttpResponse[T]]](None)
  private val pathStack: Stack[String] = new Stack[String];

  def path(path: String)(f: => Unit): Unit = {
    pathStack.push(path)

    try { f } finally { pathStack.pop() }
  }

  def get(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(),
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(GET, queryParameters, headers, None, version, f, timeout)

  def put(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T],
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(PUT, queryParameters, headers, content, version, f, timeout)

  def post(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T],
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(POST, queryParameters, headers, content, version, f, timeout)

  def delete(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(),
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(DELETE, queryParameters, headers, content, version, f, timeout)

  def options(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(), content: Option[T],
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(OPTIONS, queryParameters, headers, None, version, f, timeout)

  def head(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(),
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(HEAD, queryParameters, headers, None, version, f, timeout)

  def connect(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(),
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(CONNECT, queryParameters, headers, content, version, f, timeout)

  def trace(f: => Unit, queryParameters: Map[Symbol, String] = Map(), headers: Map[String, String] = Map(),
          version: HttpVersion = `HTTP/1.1`, timeout: Long = 60000) = custom(TRACE, queryParameters, headers, None, version, f, timeout)

  def custom(method: HttpMethod, queryParameters: Map[Symbol, String], headers: Map[String, String],
             content: Option[T], version: HttpVersion, f: => Unit, timeout: Long = 60000) = {
    val uri         = currentPath
    val handler     = handlerForRequest(uri, method)
    val parameters  = handler._1(uri) ++ queryParameters
    val future      = handler._3(HttpRequest[T](method, uri, parameters, headers, content, version))

    withResponse(waitForResponse(future, timeout), f)
  }

  private def waitForResponse(future: Future[HttpResponse[T]], timeout: Long) = {
    if (!future.isDelivered){
      val latch        = new CountDownLatch(1)

      future.deliverTo(v => {latch.countDown})

      latch.await(timeout, TimeUnit.MILLISECONDS)
    }
    future.value
  }

  private def handlerForRequest(uri: String, method: HttpMethod) = {
    val requestHandler = service.hierarchy.find(handler => handler._1.isDefinedAt(uri) && method == handler._2)
    requestHandler.getOrElse(throw new RuntimeException("Service for request cannot be found. Uri=%s; Method=%s".format(uri, method.toString)))
  }

  private def withResponse(response: Option[HttpResponse[T]], f: => Unit) = _response.withValue(response)(f)

  private def currentPath: String = pathStack.foldRight[String]("") { (element, path) => path + element }

  def response = _response.value.getOrElse(throw new RuntimeException("Request is not loaded."))
  def status   = response.status
  def headers  = response.headers
  def content  = response.content
  def version  = response.version

  def service: RestHierarchy[T]
}