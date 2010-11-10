package blueeyes.core.service

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