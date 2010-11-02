package blueeyes.core.service

import blueeyes.util.Future
import blueeyes.core.http._
import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._

trait RestHierarchy[S] {
  private type Parameters[T] = (RestPathPattern, HttpMethod, HttpRequest[T] => Future[HttpResponse[T]], HttpDataTranscoder[T, S])
  def hierarchy: List[Parameters[_]]
}

trait HttpService2[Base] extends PartialFunction[HttpRequest[Base], Future[HttpResponse[Base]]] {
  private type Handler = PartialFunction[HttpRequest[Base], Future[HttpResponse[Base]]]
  
  def name:             String
  def version:          String = majorVersion.toString + "." + String.format("%02d", int2Integer(minorVersion))
  def majorVersion:     Int = version.split(".")(0).toInt
  def minorVersion:     Int = version.split(".")(1).toInt
  
  def startupHooks:     List[() => Future[_]] = Nil
  def shutdownHooks:    List[() => Future[_]] = Nil
  def notFoundHandler:  Option[HttpNotFoundHandler[_, _, Base]] = None
  def pathMethodHandlers:  List[HttpPathMethodHandler[_, _, Base]] = Nil
  
  def start = {
    startupHooks.foreach { f => f() }
  }
  
  def shutdown = {
    shutdownHooks.foreach { f => f() }
  }
  
  def allHandlers: List[Handler] = {
    val collector: PartialFunction[AnyRef, Handler] = { case e: Handler => e }
    
    pathMethodHandlers.collect(collector) ++ notFoundHandler.toList.collect(collector)
  }
  
  def isDefinedAt(request: HttpRequest[Base]) = masterHandler.isDefinedAt(request)
  
  def apply(request: HttpRequest[Base]) = masterHandler.apply(request)
  
  override def toString = name + "-" + version
  
  private lazy val masterHandler: PartialFunction[HttpRequest[Base], Future[HttpResponse[Base]]] = {
    allHandlers.foldLeft[Handler](HttpNoHandler()) { (composite, cur) => composite.orElse(cur: Handler) }
  }
}

trait HttpServiceBuilder[Base] {
  protected type RequestHandler[In, Out] = HttpRequest[In] => Future[HttpResponse[Out]]
  protected type MethodHandler[T] = RequestHandler[T, T]
  
  def path(path: RestPathPattern)(f: => Unit): Unit
  
  def get[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(GET, handler, t)

  def put[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(PUT, handler, t)

  def post[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(POST, handler, t)

  def delete[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(DELETE, handler, t)

  def options[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(OPTIONS, handler, t)

  def head[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(HEAD, handler, t)

  def connect[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(CONNECT, handler, t)

  def trace[T](handler: MethodHandler[T])(implicit t: HttpDataTranscoder[T, Base]) = custom(TRACE, handler, t)

  def custom[T](method: HttpMethod, handler: MethodHandler[T], transcoder: HttpDataTranscoder[T, Base])
  
  def startup(f: => Future[_])
  
  def shutdown(f: => Future[_])
  
  def notFound[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): Unit
}

trait HttpServiceBuilderComposable[Base] { self =>
  import scala.collection.mutable.{Buffer, ListBuffer, Stack}
  
  def services: Buffer[HttpService2[Base]]
  
  case class service(name: String) extends HttpService2[Base] with HttpServiceBuilder[Base] {
    private val pathStack: Stack[RestPathPattern] = new Stack[RestPathPattern].push(RestPathPattern.Root);
    
    self.services += this
    
    private var _notFoundHandler: Option[HttpNotFoundHandler[_, _, Base]] = None
    
    private val _startupHooks     = new ListBuffer[() => Future[_]]
    private val _shutdownHooks    = new ListBuffer[() => Future[_]]
    private val _pathMethodHandlers  = new ListBuffer[HttpPathMethodHandler[_, _, Base]]
    
    override def startupHooks    = _startupHooks.toList
    override def shutdownHooks   = _shutdownHooks.toList
    override def notFoundHandler = _notFoundHandler
    override def pathMethodHandlers = _pathMethodHandlers.toList
    
    def path(path: RestPathPattern)(f: => Unit): Unit = {
      pathStack.push(path)

      try { f } finally { pathStack.pop() }
    }

    def custom[T](method: HttpMethod, handler: MethodHandler[T], transcoder: HttpDataTranscoder[T, Base]) = {
      _pathMethodHandlers += HttpPathMethodHandler(currentPathPattern, method, handler, transcoder.inverse, transcoder)
    }
    
    def startup(f: => Future[_]) = {
      val thunk = () => f
      
      _startupHooks += thunk
    }
    
    def shutdown(f: => Future[_]) = {
      val thunk = () => f
      
      _shutdownHooks += thunk
    }
    
    def notFound[In, Out](handler: RequestHandler[In, Out])(implicit in: HttpDataTranscoder[Base, In], out: HttpDataTranscoder[Out, Base]): Unit = _notFoundHandler match {
      case None => _notFoundHandler = Some(HttpNotFoundHandler[In, Out, Base](handler, in, out))
      
      case _ => error("Not found handler already specified")
    }
    
    private def currentPathPattern: RestPathPattern = pathStack.foldRight[RestPathPattern](RestPathPattern.Root) { (element, path) => path / element }
  }
}

trait RestHierarchyBuilder[S] extends RestHierarchy[S] {
  import scala.collection.mutable.{Stack, ArrayBuffer}
  
  private type Handler[T] = HttpRequest[T] => Future[HttpResponse[T]]
  private type Parameters[T] = (RestPathPattern, HttpMethod, Handler[T], HttpDataTranscoder[T, S])
  
  private val pathStack: Stack[RestPathPattern] = new Stack[RestPathPattern].push(RestPathPattern.Root);
  private val _hierarchy: ArrayBuffer[Parameters[_]] = new ArrayBuffer
  
  def hierarchy = _hierarchy.toList
  
  def build = hierarchy
  
  def path(path: RestPathPattern)(f: => Unit): Unit = {
    pathStack.push(path)
    
    try { f } finally { pathStack.pop() }
  }
  
  def get[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(GET, handler, t)
  
  def put[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(PUT, handler, t)
  
  def post[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(POST, handler, t)
  
  def delete[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(DELETE, handler, t)
  
  def options[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(OPTIONS, handler, t)
  
  def head[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(HEAD, handler, t)
  
  def connect[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(CONNECT, handler, t)
  
  def trace[T](handler: Handler[T])(implicit t: HttpDataTranscoder[T, S]) = custom(TRACE, handler, t)
  
  def custom[T](method: HttpMethod, handler: Handler[T], t: HttpDataTranscoder[T, S]) = {
    _hierarchy += ((currentPath, method, handler, t))
  }
  
  private def currentPath: RestPathPattern = pathStack.foldRight[RestPathPattern](RestPathPattern.Root) { (element, path) => path / element }
  //private def currentPath: RestPathPattern = { println(pathStack); pathStack.foldLeft[RestPathPattern](RestPathPattern.Root) { (path, element) => path / element } }
}

import blueeyes.core.data._
import blueeyes.core.http.MimeType
import blueeyes.core.http.MimeTypes._
import blueeyes.json.JsonAST.JValue

sealed trait HttpResponseType[T]

case object HttpResponseStringType extends HttpResponseType[String]
case object HttpResponseBytesType  extends HttpResponseType[Array[Byte]]

trait HttpDataTranscoder[T, S] extends DataTranscoder[T, S] { self =>
  def responseType: HttpResponseType[S]
  
  def inverse = new HttpDataTranscoder[S, T] {
    def transcode = self.transcode.inverse
    
    def responseType = error("bad design")
    
    def mimeType = self.mimeType
  }
}

class HttpStringDataTranscoder[T](transcode: Bijection[T, String], mimeType: MimeType) extends DataTranscoderImpl[T, String](transcode, mimeType) with HttpDataTranscoder[T, String]{
  val responseType: HttpResponseType[String] = HttpResponseStringType
}
class HttpBytesDataTranscoder[T](transcode: Bijection[T, Array[Byte]], mimeType: MimeType) extends DataTranscoderImpl[T, Array[Byte]](transcode, mimeType) with HttpDataTranscoder[T, Array[Byte]]{
  val responseType: HttpResponseType[Array[Byte]] = HttpResponseBytesType
}

object Transcoders{
  implicit val HttpJsonToText = new HttpStringDataTranscoder[JValue](JsonToTextBijection, application/json)
}
