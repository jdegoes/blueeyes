package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler

import scala.util.parsing.combinator._
import scala.util.parsing.input._

/*
Usage: 

import MimeTypes._
val mimeType = image/gif
val imageMimeType = image/gif 
val javaScriptMimeType = text/javascript
*/

sealed trait MimeType {
  def maintype: String
  def subtype: String
  
  def value: String = maintype + "/" + subtype
  
  def extensions: List[String] = subtype :: Nil
  
  def defaultExtension = extensions.head
  
  override def hashCode = value.hashCode
  
  override def equals(o: Any) = o match {
    case that: MimeType => this.value.toLowerCase == that.value.toLowerCase
    
    case _ => false
  }

  override def toString = value
}

object MimeTypes extends RegexParsers {
  private def elementParser: Parser[Option[MimeType]] =
      (("*" ~> "/" <~ "*"                            ^^^ anymaintype / anysubtype |
      "application" ~> "/" <~ "*"                   ^^^ application / anysubtype |
      "application" ~> "/" <~ "javascript"          ^^^ application / javascript |
      "text"        ~> "/" <~ "javascript"          ^^^ text / javascript |
      "application" ~> "/" <~ "x-javascript"        ^^^ application / `x-javascript` |
      "application" ~> "/" <~ "soap+xml"            ^^^ application / `soap+xml` |
      "application" ~> "/" <~ "xhtml+xml"           ^^^ application / `xhtml+xml` |
      "application" ~> "/" <~ "xml-dtd"             ^^^ application / `xml-dtd` |
      "application" ~> "/" <~ "json"                ^^^ application / json |
      "application" ~> "/" <~ "x-latex"             ^^^ application / `x-latex` |
      "application" ~> "/" <~ "octet-stream"        ^^^ application / `octet-stream` |
      "application" ~> "/" <~ "ogg"                 ^^^ application / ogg |
      "application" ~> "/" <~ "pdf"                 ^^^ application / `pdf` |
      "application" ~> "/" <~ "postscript"          ^^^ application / `postscript` |
      "application" ~> "/" <~ "x-dvi"               ^^^ application / `x-dvi` |
      "application" ~> "/" <~ "x-shockwave-flash"   ^^^ application / `x-shockwave-flash` |
      "application" ~> "/" <~ "x-tar"               ^^^ application / `x-tar` |
      "application" ~> "/" <~ "x-ttf"               ^^^ application / `x-ttf` |
      "application" ~> "/" <~ "zip"                 ^^^ application / `zip` |
      "audio"       ~> "/" <~ "basic"               ^^^ audio / basic |
      "audio"       ~> "/" <~ "mp4"                 ^^^ audio / mp4 |
      "audio"       ~> "/" <~ "midi"                ^^^ audio / midi |
      "audio"       ~> "/" <~ "mpeg"                ^^^ audio / mpeg |
      "audio"       ~> "/" <~ "vorbis"              ^^^ audio / vorbis |
      "audio"       ~> "/" <~ "x-ms-wma"            ^^^ audio / `x-ms-wma` |
      "audio"       ~> "/" <~ "x-ms-wax"            ^^^ audio / `x-ms-wax` |
      "audio"       ~> "/" <~ "x-realaudio"         ^^^ audio / `x-realaudio` |
      "audio"       ~> "/" <~ "x-wav"               ^^^ audio / `x-wav` |
      "image"       ~> "/" <~ "gif"                 ^^^ image / gif |
      "image"       ~> "/" <~ "png"                 ^^^ image / png |
      "image"       ~> "/" <~ "jpeg"                ^^^ image / jpeg |
      "image"       ~> "/" <~ "svg+xml"             ^^^ image / `svg+xml` |
      "image"       ~> "/" <~ "tiff"                ^^^ image / tiff |
      "image"       ~> "/" <~ "vnd.microsoft.icon"  ^^^ image / `vnd.microsoft.icon` |
      "multipart"   ~> "/" <~ "mixed"               ^^^ multipart / mixed |
      "multipart"   ~> "/" <~ "alternative"         ^^^ multipart / alternative |
      "multipart"   ~> "/" <~ "related"             ^^^ multipart / related |
      "multipart"   ~> "/" <~ "form-data"           ^^^ multipart / `form-data` |
      "multipart"   ~> "/" <~ "signed"              ^^^ multipart / signed |
      "multipart"   ~> "/" <~ "encrypted"           ^^^ multipart / encrypted |
      "text"        ~> "/" <~ "css"                 ^^^ text / css |
      "text"        ~> "/" <~ "csv"                 ^^^ text / csv |
      "text"        ~> "/" <~ "html"                ^^^ text / html |
      "text"        ~> "/" <~ "plain"               ^^^ text / plain |
      "text"        ~> "/" <~ "x-c"                 ^^^ text / `x-c` |
      "text"        ~> "/" <~ "xml"                 ^^^ text / xml |
      "video"       ~> "/" <~ "quicktime"           ^^^ video / quicktime |
      "video"       ~> "/" <~ "x-msvideo"           ^^^ video / `x-msvideo`)?) <~ regex("""[^,]*""".r) ^^ {case v => v}

  private def parser = repsep(elementParser, regex("""[ ]*,[ ]*""".r)) ^^ {case values => values.filter(_ != None).map(_.get) }

  def parseMimeTypes(inString: String): List[MimeType] = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => error("The mimeTypes " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
  }

  trait GenericType extends ProductPrefixUnmangler {
    def subtype = unmangledName
  }

  sealed abstract class StarType(val extensions: List[String]) extends GenericType
  sealed abstract class ApplicationType(val extensions: List[String]) extends GenericType
  sealed abstract class AudioType(val extensions: List[String]) extends GenericType
  sealed abstract class ImageType(val extensions: List[String]) extends GenericType
  sealed abstract class MessageType(val extensions: List[String]) extends GenericType
  sealed abstract class MultipartType(val extensions: List[String]) extends GenericType
  sealed abstract class PrsType(val extensions: List[String]) extends GenericType
  sealed abstract class TextType(val extensions: List[String]) extends GenericType
  sealed abstract class VideoType(val extensions: List[String]) extends GenericType

  /* Star Type */
  case object anysubtype extends StarType(Nil) {
    override def subtype = "*"
  }

  /* Application Types */
  sealed abstract class JavaScriptApplicationType extends ApplicationType("js" :: Nil)
  sealed abstract class OggApplicationType extends ApplicationType("ogg" :: Nil)

  case object javascript extends JavaScriptApplicationType
  case object `x-javascript` extends JavaScriptApplicationType
  case object `soap+xml` extends ApplicationType("soap+xml" :: Nil)
  case object `xhtml+xml` extends ApplicationType("xhtml+xml" :: Nil)
  case object `xml-dtd` extends ApplicationType("xml-dtd" :: Nil)

  case object json extends ApplicationType("json" :: Nil)
  case object `x-latex` extends ApplicationType("latex" :: Nil)
  case object `octet-stream` extends ApplicationType("bin" :: "class" :: "dms" :: "exe" :: "lha" :: "lzh" :: Nil)
  case object ogg extends JavaScriptApplicationType
  case object pdf extends ApplicationType("pdf" :: Nil)
  case object postscript extends ApplicationType("ai" :: Nil)
  case object `x-dvi` extends ApplicationType("dvi" :: Nil)
  case object `x-shockwave-flash` extends ApplicationType("swf" :: Nil)
  case object `x-tar` extends ApplicationType("tar" :: Nil)
  case object `x-ttf` extends ApplicationType("ttf" :: Nil)
  case object zip extends ApplicationType("zip" :: Nil)

  /* Audio Types */
  sealed abstract class MpegAudioType extends AudioType("mpg" :: "mpeg" :: "mpga" :: "mpe" :: "mp3" :: "mp2" :: Nil)
  sealed abstract class Mp4AudioType extends AudioType("mp4" :: Nil)

  case object basic extends AudioType("au" :: "snd" :: Nil)
  case object mp4 extends Mp4AudioType
  case object midi extends AudioType("midi" :: "mid" :: "kar" :: Nil)
  case object mpeg extends MpegAudioType
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
  case object html extends TextType("html" :: "htm" :: Nil)
  case object plain extends TextType("c" :: "c++" :: "cc" :: "com" :: "conf" :: "f" :: "h" :: "jav" :: "pl" :: "text" :: "txt" :: Nil)
  case object `x-c` extends TextType("c" :: Nil)
  case object xml extends TextType("xml" :: Nil)

  /* Video Types */
  case object quicktime extends VideoType("qt" :: "mov" :: Nil)
  case object `x-msvideo` extends VideoType("avi" :: Nil)

  /* Implicit Conversions */
  implicit def applicationTypeJavaScript2TextTypeJavaScript(appType: JavaScriptApplicationType): TextType = {
    case object TextTypeJavaScript extends TextType(appType.extensions) { 
      override def subtype = appType.subtype 
    }
    return TextTypeJavaScript
  }

  implicit def applicationTypeOgg2AudioTypeOgg(appType: OggApplicationType ): AudioType = {
    case object AudioTypeOgg extends AudioType(appType.extensions) {
      override def subtype = appType.subtype
    }
    return AudioTypeOgg
  }

  implicit def applicationTypeOgg2VideoTypeOgg(appType: OggApplicationType): VideoType = {
    case object VideoTypeOgg extends VideoType(appType.extensions) {
      override def subtype = appType.subtype 
    }
    return VideoTypeOgg
  }

  implicit def audioTypeMpeg2VideoTypeMpeg (audioType: MpegAudioType): VideoType = {
    case object VideoTypeMpeg extends VideoType(audioType.extensions) {
      override def subtype = audioType.subtype
    }
    return VideoTypeMpeg
  }

  implicit def audioTypeMp42VideoTypeMp4 (audioType: Mp4AudioType): VideoType = {
    case object VideoTypeMp4 extends VideoType(audioType.extensions) {
      override def subtype = audioType.subtype
    }
    return VideoTypeMp4
  }

  /* Implicit conversions for Star (*) */

  implicit def starTypeStar2ApplicationTypeStar (starType: StarType): ApplicationType = {
    case object ApplicationTypeStar extends ApplicationType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return ApplicationTypeStar 
  }

  implicit def starTypeStar2AudioTypeStar (starType: StarType): AudioType = {
    case object AudioTypeStar extends AudioType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return AudioTypeStar 
  }

  implicit def starTypeStar2ImageTypeStar (starType: StarType): ImageType = {
    case object ImageTypeStar extends ImageType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return ImageTypeStar 
  }

  implicit def starTypeStar2MessageTypeStar (starType: StarType): MessageType = {
    case object MessageTypeStar extends MessageType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return MessageTypeStar 
  }

  implicit def starTypeStar2MultipartTypeStar (starType: StarType): MultipartType = {
    case object MultipartTypeStar extends MultipartType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return MultipartTypeStar 
  }

  implicit def starTypeStar2PrsTypeStar (starType: StarType): PrsType = {
    case object PrsTypeStar extends PrsType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return PrsTypeStar 
  }

  implicit def starTypeStar2TextTypeStar (starType: StarType): TextType = {
    case object TextTypeStar extends TextType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return TextTypeStar 
  }

  implicit def starTypeStar2VideoTypeStar (starType: StarType): VideoType = {
    case object VideoTypeStar extends VideoType(starType.extensions) {
      override def subtype = starType.subtype
    }
    return VideoTypeStar 
  }


  /* Constructor Methods */
  object anymaintype {
    def / (starType: StarType) = new MimeType {
      def maintype = "*"
      def subtype = starType.subtype
      override def extensions = starType.extensions
    }
  }

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

  object video {
    def / (videoType: VideoType) = new MimeType {
      def maintype = "video"
      def subtype = videoType.subtype
      override def extensions = videoType.extensions
    }
  }
}


