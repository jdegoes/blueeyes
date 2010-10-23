package blueeyes.core.http

import scala.util.matching.Regex

sealed trait Expectation {
  def code: HttpStatusCode 
  def value: Int = code.value
  override def toString: String = value.toString + "-" + code.name.toLowerCase
}

object Expectations {

  def parseExpectations(inString: String): Expectation = {
    def CodeRegex = new Regex("""\d{3}""")
    def outExpectation = CodeRegex.findFirstIn(inString.trim).getOrElse("") match {
      case "100"  => continue 
      case "417"  => failure 
      case _      => failure 
    }
    return outExpectation
  }

  sealed abstract class ExpectType(inCode: HttpStatusCode) extends Expectation {
    override def code = inCode;
  }

  case object continue extends ExpectType (HttpStatusCodes.Continue) 
  case object failure extends ExpectType (HttpStatusCodes.ExpectationFailed) 

}
