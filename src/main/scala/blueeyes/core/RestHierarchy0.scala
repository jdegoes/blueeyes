package blueeyes.core.service

import blueeyes.core.http.HttpMethods._
import blueeyes.core.http.{HttpMethod}
import blueeyes.util.Future
import blueeyes.core.data.{Bijection, TextToTextBijection, DataTranscoderImpl, DataTranscoder}

trait RestHierarchy0 {
  private type Parameters[T, S] = (RestPathPattern, HttpMethod, HttpRequest[T] => Future[HttpResponse[T]], HttpDataTranscoder0[T, S])
  def hierarchy: List[Parameters[_, _]]
}

trait RestHierarchyBuilder0 extends RestHierarchy0 {
  import scala.collection.mutable.{Stack, ArrayBuffer}

  private type Handler[T] = HttpRequest[T] => Future[HttpResponse[T]]
  private type Parameters[T, S] = (RestPathPattern, HttpMethod, Handler[T], HttpDataTranscoder0[T, S])

  private val pathStack: Stack[RestPathPattern] = new Stack[RestPathPattern].push(RestPathPattern.Root);
  private val _hierarchy: ArrayBuffer[Parameters[_, _]] = new ArrayBuffer

  def hierarchy = _hierarchy.toList

  def path(path: RestPathPattern)(f: => Unit): Unit = {
    pathStack.push(path)

    try { f } finally { pathStack.pop() }
  }

  def get[T, S](handler: Handler[T])(implicit t: HttpDataTranscoder0[T, S]) = custom(GET, handler, t)

  def custom[T, S](method: HttpMethod, handler: Handler[T], t: HttpDataTranscoder0[T, S]) = {
    _hierarchy += ((currentPath, method, handler, t))
  }

  private def currentPath: RestPathPattern = pathStack.foldRight[RestPathPattern](RestPathPattern.Root) { (element, path) => path / element }
  //private def currentPath: RestPathPattern = { println(pathStack); pathStack.foldLeft[RestPathPattern](RestPathPattern.Root) { (path, element) => path / element } }
}

import blueeyes.core.service.RestPathPatternImplicits._
import blueeyes.core.http.MimeTypes._
import blueeyes.core.http.MimeType
class SampleService0 extends RestHierarchyBuilder0 {
  implicit val dd = HttpStringDataTranscoder0(TextToTextBijection, text / plain)
  path("/get/'foo") {get[String, String](new GetHandler())}

  class GetHandler extends Function1[HttpRequest[String], Future[HttpResponse[String]]]{
    def apply(request: HttpRequest[String]) = {
      val response = HttpResponse[String]()
      new Future[HttpResponse[String]]().deliver(response)
    }
  }
}


sealed trait HttpResponseType0[T]

case object HttpResponseStringType0 extends HttpResponseType0[String]
case object HttpResponseByteType0   extends HttpResponseType0[Array[Byte]]

trait HttpDataTranscoder0[T, S] extends DataTranscoder[T, S]{
  def responseType: HttpResponseType0[S]
}

case class HttpStringDataTranscoder0[T](val transcode1: Bijection[T, String], val mimeType1: MimeType) extends DataTranscoderImpl[T, String](transcode1, mimeType1) with HttpDataTranscoder0[T, String]{
  def responseType: HttpResponseType0[String] = HttpResponseStringType0
}
