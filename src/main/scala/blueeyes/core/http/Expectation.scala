package blueeyes.core.http

import scala.util.matching.Regex

sealed trait Expectation {
  def code: HttpStatusCode 
  def value: Int = code.value
  override def toString: String = value.toString + "-" + code.name.toLowerCase
}

object Expectations {

  def parseExpectations(inString: String): Option[Expectation] = {
    def CodeRegex = new Regex("""\d{3}""")
    def outExpectation: Option[Expectation] = CodeRegex.findFirstIn(inString.trim).getOrElse("") match {
      case "100"  => Some(continue)
      case "417"  => Some(failure)
      case _      => return None
    }
    return outExpectation
  }

  sealed abstract class ExpectType(inCode: HttpStatusCode) extends Expectation {
    override def code = inCode;
  }

  case object continue extends ExpectType (HttpStatusCodes.Continue) 
  case object failure extends ExpectType (HttpStatusCodes.ExpectationFailed) 

}
