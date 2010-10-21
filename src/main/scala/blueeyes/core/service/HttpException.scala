package blueeyes.core.service

import blueeyes.core.http.HttpStatusCode
import blueeyes.core.http.HttpFailure

case class HttpException(failure: HttpFailure, reason: String) extends Exception(reason) {
  def this(failure: HttpFailure) = this(failure, failure.defaultMessage)
}
