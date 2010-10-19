package blueeyes.core.service

import scala.util.matching.Regex

/*
Usage: 

import MimeTypes._
val mimeType = image/gif

*/

sealed trait MimeType {
  def maintype: String
  def subtype: String
  
  def value: String = maintype + "/" + subtype
  
  def extensions: List[String] = subtype :: Nil
  
  def defaultExtension = extensions.head
}

object MimeTypes {
  trait GenericType extends Product {
    def subtype = productPrefix 
  }

  sealed abstract class ApplicationType(val extensions: List[String]) extends GenericType
  sealed abstract class AudioType(val extensions: List[String]) extends GenericType
  sealed abstract class ImageType(val extensions: List[String]) extends GenericType
  sealed abstract class MessageType(val extensions: List[String]) extends GenericType
  sealed abstract class MultipartType(val extensions: List[String]) extends GenericType
  sealed abstract class PrsType(val extensions: List[String]) extends GenericType
  sealed abstract class TextType(val extensions: List[String]) extends GenericType
  sealed abstract class VideoType(val extensions: List[String]) extends GenericType

  /* Application Types */
  //object javascript extends ApplicationType("js" :: Nil)

  case object `soap+xml` extends ApplicationType("soap+xml" :: Nil)
  case object `xhtml+xml` extends ApplicationType("xhtml+xml" :: Nil)
  case object `xml-dtd` extends ApplicationType("xml-dtd" :: Nil)

  case object json extends ApplicationType("json" :: Nil)
  case object `x-latex` extends ApplicationType("latex" :: Nil)
  case object `octect-stream` extends ApplicationType("bin" :: "class" :: "dms" :: "exe" :: "lha" :: "lzh" :: Nil)
  case object pdf extends ApplicationType("pdf" :: Nil)
  case object postscript extends ApplicationType("ai" :: Nil)
  case object `x-dvi` extends ApplicationType("dvi" :: Nil)
  case object `x-shockwave-flash` extends ApplicationType("swf" :: Nil)
  case object `x-tar` extends ApplicationType("tar" :: Nil)
  case object `x-ttf` extends ApplicationType("ttf" :: Nil)
  case object zip extends ApplicationType("zip" :: Nil)

  /* Audio Types */
  case object basic extends AudioType("au" :: "snd" :: Nil)
  case object mp4 extends AudioType("mp4" :: Nil)
  case object midi extends AudioType("midi" :: "mid" :: "kar" :: Nil)
  case object ogg extends AudioType("ogg" :: Nil)
  case object vorbis extends AudioType("vorbis" :: Nil)
  case object `x-ms-wma` extends AudioType("wma" :: Nil)
  case object `x-ms-wax` extends AudioType("wax" :: Nil)
  case object `x-realaudio` extends AudioType("ra" :: Nil)
  case object `x-wav` extends AudioType("wav" :: Nil)

  /* Image Types */ 
  case object gif extends ImageType("gif" :: Nil)
  case object png extends ImageType("png" :: Nil)
  case object jpeg extends ImageType("jpg" :: "jpeg" :: "jpe" :: Nil)
  case object `svg+xml` extends ImageType("svg+xml" :: Nil)
  case object tiff extends ImageType("tiff" :: Nil)
  case object `vnd.microsoft.icon` extends ImageType("ico" :: Nil)

  /* Message Types */
  case object http extends MessageType("http" :: Nil)
  case object `delivery-status` extends MessageType("delivery-status" :: Nil)

  /* Multipart Types */
  case object mixed extends MultipartType("mixed" :: Nil)
  case object alternative extends MultipartType("alternative" :: Nil)
  case object related extends MultipartType("related" :: Nil)
  case object `form-data` extends MultipartType("form-data" :: Nil)
  case object signed extends MultipartType("signed" :: Nil)
  case object encrypted extends MultipartType("encrypted" :: Nil)

  /* Text Types */
  case object css extends TextType("css" :: Nil)
  case object csv extends TextType("csv" :: Nil)
  case object javascript extends TextType("js" :: Nil)
  case object html extends TextType("html" :: "htm" :: Nil)
  case object plain extends TextType("c" :: "c++" :: "cc" :: "com" :: "conf" :: "f" :: "h" :: "jav" :: "pl" :: "text" :: "txt" :: Nil)
  case object xml extends TextType("xml" :: Nil)

  /* Video Types */
  case object mpeg extends AudioType("mpg" :: "mpeg" :: "mpga" :: "mpe" :: Nil)
  case object quicktime extends VideoType("qt" :: "mov" :: Nil)
  case object `x-msvideo` extends VideoType("avi" :: Nil)

  /* Constructor Methods */
  object application {
    def / (applicationType: ApplicationType) = new MimeType {
      def maintype = "application"
      def subtype = applicationType.subtype 
      override def extensions = applicationType.extensions
    }
  }

  object audio {
    def / (audioType: AudioType) = new MimeType {
      def maintype = "audio"
      def subtype = audioType.subtype 
      override def extensions = audioType.extensions
    }
  }

  object image {
    def / (imageType: ImageType) = new MimeType {
      def maintype = "image"
      def subtype = imageType.subtype 
      override def extensions = imageType.extensions
    }
  }

  object message {
    def / (messageType: MessageType) = new MimeType {
      def maintype = "message"
      def subtype = messageType.subtype 
      override def extensions = messageType.extensions
    }
  }

  object multipart {
    def / (multipartType: MultipartType) = new MimeType {
      def maintype = "multipart"
      def subtype = multipartType.subtype 
      override def extensions = multipartType.extensions
    }
  }

  object text {
    def / (textType: TextType) = new MimeType {
      def maintype = "text"
      def subtype = textType.subtype 
      override def extensions = textType.extensions
    }
  }
}
// val imageMimeType = image/gif 
// val javaScriptMimeType = text/javascript


