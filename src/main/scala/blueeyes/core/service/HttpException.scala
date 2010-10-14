package blueeyes.core.service

case class HttpException(failure: HttpFailure, reason: String) extends Exception(reason) {
  def this(failure: HttpFailure) = this(failure, failure.defaultMessage)
}