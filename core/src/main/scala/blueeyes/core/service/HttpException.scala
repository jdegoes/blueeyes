package blueeyes.core.service

case class HttpException(failure: HttpFailure, reason: String) extends Exception(reason) {
  def apply(failure: HttpFailure) = new HttpException(failure, failure.defaultMessage)
}