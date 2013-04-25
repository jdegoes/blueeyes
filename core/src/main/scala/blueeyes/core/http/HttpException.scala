package blueeyes.core.http

import blueeyes.util.RichThrowableImplicits._

case class HttpException(failure: HttpFailure, reason: String) extends Exception(reason) 

object HttpException {
  def apply(failure: HttpFailure): HttpException = new HttpException(failure, failure.defaultMessage)
}
