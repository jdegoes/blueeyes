package blueeyes.core.service

import blueeyes.core.http.HttpStatusCode
import blueeyes.core.http.HttpFailure

case class HttpException(failure: HttpFailure, reason: String) extends Exception(reason)

object HttpException {
  def apply(failure: HttpFailure): HttpException = new HttpException(failure, failure.defaultMessage)
}