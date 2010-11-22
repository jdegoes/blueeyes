package blueeyes.core.http

import blueeyes.util.RichThrowableImplicits._

case class HttpException(failure: HttpFailure, reason: String) extends Exception(reason) {
  def status = HttpStatus(failure, reason)
}

object HttpException {
  def apply(failure: HttpFailure): HttpException = new HttpException(failure, failure.defaultMessage)
  
  def apply(failure: HttpFailure, cause: Throwable): HttpException = new HttpException(failure, cause.getMessage)
}