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
  def parseWarnings(inString: String): List[WarningInfo] = {

    def outWarnings = inString.trim.split(",").map(_.trim.split(" ")).map{ x => 
      val agent = x(1)
      x(0) match {
        case "110" => Response(agent, None)
        case "111" => Revalidation(agent, None)
        case "112" => Disconnected(agent, None)
        case "113" => Heuristic(agent, None)
        case "119" => Miscellaneous(agent, None)
        case "214" => Transformation(agent, None)
        case "299" => MiscellaneousPersistent(agent, None)
      }
    }

    return outWarnings.toList
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

