package blueeyes.core.http

import blueeyes.core.data._
import blueeyes.core.http.MimeTypes._
import blueeyes.json.JsonAST.JValue

trait HttpDataTranscoder[T, S]{ self =>
  def transcode: Bijection[T, S]
  
  def mimeType: MimeType
}

class HttpStringDataTranscoder[T](val transcode: Bijection[T, String], val mimeType: MimeType) extends HttpDataTranscoder[T, String]

class HttpBytesDataTranscoder[T](val transcode: Bijection[T, Array[Byte]], val mimeType: MimeType) extends HttpDataTranscoder[T, Array[Byte]]

object Transcoders{
  implicit val HttpJsonToText = new HttpStringDataTranscoder[JValue](Bijections.JValueToString, application/json)
}