package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait Expectation {
  def code: HttpStatusCode 
  def value: Int = code.value
  override def toString: String = value.toString + "-" + code.name.toLowerCase
}

object Expectations extends RegexParsers {

  private def parser = (
    "100" ^^^ continue |
    "417" ^^^ failure
  )?

  def parseExpectations(inString: String): Option[ExpectType] = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => sys.error("The Expectations " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  sealed abstract class ExpectType(inCode: HttpStatusCode) extends Expectation {
    override def code = inCode;
  }

  case object continue extends ExpectType (HttpStatusCodes.Continue) 
  case object failure extends ExpectType (HttpStatusCodes.ExpectationFailed) 

}
