package blueeyes.core.http

class HttpStatus private (val code: HttpStatusCode, unsafeReason: String) {
  val reason = unsafeReason.replaceAll("[\\r\\n]", " ")
  
  override def toString = code.toString + " " + reason
  
  override def hashCode = toString.hashCode
  
  override def equals(x: Any) = x match {
    case that: HttpStatus => this.reason == that.reason && this.code == that.code
    
    case _ => false
  }
}
object HttpStatus {
  def apply(code: HttpStatusCode): HttpStatus = apply(code, code.defaultMessage)
  
  def apply(code: HttpStatusCode, reason: String): HttpStatus = new HttpStatus(code, reason)
}

trait HttpStatusImplicits {
  implicit def statusCode2HttpStatus(code: HttpStatusCode) = HttpStatus(code, code.defaultMessage)
}
object HttpStatusImplicits extends HttpStatusImplicits
