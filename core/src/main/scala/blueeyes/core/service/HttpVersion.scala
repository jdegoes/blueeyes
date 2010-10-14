package blueeyes.core.service

sealed case class HttpVersion(value: String) {
  override def toString = value
}

object HttpVersions {
  object Http_1_0 extends HttpVersion("HTTP/1.0")
  object Http_1_1 extends HttpVersion("HTTP/1.1")
}