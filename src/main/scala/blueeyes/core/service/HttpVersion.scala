package blueeyes.core.service

sealed abstract class HttpVersion extends Product {
  override def toString = productPrefix
  
  def value = productPrefix
}

object HttpVersions {
  case object `HTTP/1.0` extends HttpVersion
  case object `HTTP/1.1` extends HttpVersion
}