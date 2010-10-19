package blueeyes.core.service

sealed trait MimeType {
  def level1: String
  def level2: String

  def value: String = level1 + "/" + level2
  
  def extensions: List[String] = level2 :: Nil
  
  def defaultExtension = extensions.head
}

object MimeTypes {
  sealed case class ImageType(level2: String, extensions: List[String])

  object gif extends ImageType("gif", "gif" :: Nil)
  object png extends ImageType("png", "png" :: Nil)
  object jpeg extends ImageType("jpeg", "jpg" :: "jpeg" :: Nil)
  
  object image {
    def / (imageType: ImageType) = new MimeType {
      def level1 = "image"
      def level2 = imageType.level2
      
      override def extensions = imageType.extensions
    }
  }

  sealed case class ApplicationType(level2: String)
  object ApplicationAtomXml         extends ApplicationType("atom+xml")
  object ApplicationFormUrlEncoded  extends ApplicationType("x-www-form-urlencoded")
  object ApplicationJson            extends ApplicationType("json")
  object ApplicationOctetStream     extends ApplicationType("octet-stream")
  object ApplicationSvgXml          extends ApplicationType("svg+xml")
  object ApplicationXml             extends ApplicationType("xhtml+xml")
  object ApplicationXHtml           extends ApplicationType("xml")

  object application{
    def / (applicationType: ApplicationType) = new MimeType {
      def level1 = "application"
      def level2 = applicationType.level2
    }
  }

  sealed case class MultipartType(level2: String)
  object MultipartFormData extends MultipartType("form-data")

  object multipart{
    def / (multipartType: MultipartType) = new MimeType {
      def level1 = "multipart"
      def level2 = multipartType.level2
    }
  }
  sealed case class TextType(level2: String)
  object TextHtml   extends TextType("html")
  object TextPlain  extends TextType("plain")
  object TextXml    extends TextType("xml")

  object text{
    def / (textType: TextType) = new MimeType {
      def level1 = "text"
      def level2 = textType.level2
    }
  }

  object wildcard extends MimeType{
    def level1 = "*"
    def level2 = "*"
  }
}
// val imageMimeType = image/gif 
// val javaScriptMimeType = text/javascript


/*


import MimeTypes._

val mimeType = image/gif


*/