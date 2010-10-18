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
}
// val imageMimeType = image/gif 
// val javaScriptMimeType = text/javascript


/*


import MimeTypes._

val mimeType = image/gif


*/