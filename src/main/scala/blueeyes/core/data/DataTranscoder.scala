package blueeyes.core.data

import blueeyes.json.JsonAST.JValue
import blueeyes.core.http.MimeType
import blueeyes.core.http.MimeTypes._

import scala.xml.NodeSeq

case class DataTranscoder[T, S](transcode: Bijection[T, S], mimeType: MimeType) {
  def inverse = DataTranscoder[S, T](transcode.inverse, mimeType)
}

object DataTranscoders {
  val JValueToString    = DataTranscoder[JValue, String]      (Bijections.JValueToString,     application/json)
  val JValueToByteArray = DataTranscoder[JValue, Array[Byte]] (Bijections.JValueToByteArray,  application/json)
  val JValueToJValue    = DataTranscoder[JValue, JValue]      (Bijections.JValueToJValue,     application/json)
  
  val XMLToString       = DataTranscoder[NodeSeq, String]      (Bijections.XMLToString,        text/xml)
  val XMLToByteArray    = DataTranscoder[NodeSeq, Array[Byte]] (Bijections.XMLToByteArray,     text/xml)
  val XMLToXML          = DataTranscoder[NodeSeq, NodeSeq]     (Bijections.XMLToXML,           text/xml)
  
  val StringToString    = DataTranscoder[String, String]      (Bijections.StringToString,     text/plain)
  val StringToByteArray = DataTranscoder[String, Array[Byte]] (Bijections.StringToByteArray,  text/plain)  
  val StringToJValue    = JValueToString.inverse
  val StringToXML       = XMLToString.inverse
}
