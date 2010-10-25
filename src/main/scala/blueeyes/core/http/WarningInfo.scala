package blueeyes.core.http

sealed trait WarningInfo {

  def statusCode: HttpStatusCode

  def warnAgent: String

  def date: Option[HttpDateTime]

  def value: String = (statusCode.value.toString :: warnAgent :: statusCode.defaultMessage :: date.toString :: Nil).mkString(" ")

  override def toString = value

}


object Warnings {

  case class Response(warnAgent: String, date: Option[HttpDateTime]) extends WarningInfo {
    def statusCode = HttpStatusCodes.Response
  }

  case class Revalidation(warnAgent: String, date: Option[HttpDateTime]) extends WarningInfo {
    def statusCode = HttpStatusCodes.Revalidation 
  }

  case class Disconnected(warnAgent: String, date: Option[HttpDateTime]) extends WarningInfo {
    def statusCode = HttpStatusCodes.Disconnected
  }

  case class Heuristic(warnAgent: String, date: Option[HttpDateTime])  extends WarningInfo {
    def statusCode = HttpStatusCodes.Heuristic
  }

  case class Miscellaneous(warnAgent: String, date: Option[HttpDateTime])  extends WarningInfo {
    def statusCode = HttpStatusCodes.Miscellaneous 
  }

  case class Transformation(warnAgent: String, date: Option[HttpDateTime])  extends WarningInfo {
    def statusCode = HttpStatusCodes.Transformation 
  }

  case class HMiscellaneousPersistent(warnAgent: String, date: Option[HttpDateTime])  extends WarningInfo {
    def statusCode = HttpStatusCodes.MiscellaneousPersistent 
  }
}

