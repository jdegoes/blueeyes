package blueeyes.core.service

case class HttpStatus(code: HttpStatusCode, reason: String) {
  def this(code: HttpStatusCode) = this(code, code.defaultMessage)
}