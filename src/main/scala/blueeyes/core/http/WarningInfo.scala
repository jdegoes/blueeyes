package blueeyes.core.http

sealed trait WarningInfo {

  def statusCode: HttpStatusCode

  def warnAgent: String

  def date: Option[HttpDateTime]

  def value: String = (statusCode.value.toString.toList ++ warnAgent.toList ++ 
                       statusCode.defaultMessage.toList ++ date.toString.toList).mkString(" ")

  override def toString = value

}


object WarningInfos {

  /* Note about parsing warnings: It's really hard to extract the date! */
  def parseWarnings(inString: String): Array[WarningInfo] = {

    def outWarnings: Array[WarningInfo] = inString.trim.split(",").map(_.trim.split(" ")).map(x => x(0) :: x(1) :: Nil match {
      case List("110", agent) => Response(agent, None)
      case List("111", agent) => Revalidation(agent, None)
      case List("112", agent) => Disconnected(agent, None)
      case List("113", agent) => Heuristic(agent, None)
      case List("119", agent) => Miscellaneous(agent, None)
      case List("214", agent) => Transformation(agent, None)
      case List("299", agent) => MiscellaneousPersistent(agent, None)
    })

    return outWarnings
  }

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

  case class MiscellaneousPersistent(warnAgent: String, date: Option[HttpDateTime])  extends WarningInfo {
    def statusCode = HttpStatusCodes.MiscellaneousPersistent 
  }
}

