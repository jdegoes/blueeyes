package blueeyes.core.service

import blueeyes.core.http.{HttpStatusCode}

case class HttpStatus(code: HttpStatusCode, reason: String)
object HttpStatus {
  def apply(code: HttpStatusCode) = new HttpStatus(code, code.defaultMessage)
}

trait HttpStatusImplicits {
  implicit def statusCode2HttpStatus(code: HttpStatusCode) = HttpStatus(code, code.defaultMessage)
}
object HttpStatusImplicits extends HttpStatusImplicits
