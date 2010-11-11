package blueeyes.core.service

import blueeyes.core.data._
import blueeyes.core.http.MimeType
import blueeyes.core.http.MimeTypes._
import blueeyes.json.JsonAST.JValue

sealed trait HttpResponseType[T]

case object HttpResponseStringType extends HttpResponseType[String]
case object HttpResponseBytesType  extends HttpResponseType[Array[Byte]]

trait HttpDataTranscoder[T, S]{ self =>
  def responseType: HttpResponseType[S]
  
  def transcode: Bijection[T, S]
  
  def mimeType: MimeType
}

class HttpStringDataTranscoder[T](val transcode: Bijection[T, String], val mimeType: MimeType) extends HttpDataTranscoder[T, String]{
  val responseType: HttpResponseType[String] = HttpResponseStringType
}
class HttpBytesDataTranscoder[T](val transcode: Bijection[T, Array[Byte]], val mimeType: MimeType) extends HttpDataTranscoder[T, Array[Byte]]{
  val responseType: HttpResponseType[Array[Byte]] = HttpResponseBytesType
}

object Transcoders{
  implicit val HttpJsonToText = new HttpStringDataTranscoder[JValue](Bijections.JValueToString, application/json)
}