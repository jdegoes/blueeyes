package blueeyes.parsers

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import RegularExpressionAST._

object RegularExpressionPatten extends PartialFunction[String, List[RegexAtom]] with RegularExpressionGrammar with RegexParsers{
  def isDefinedAt(s: String): Boolean = regularExpression.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => true

    case _ => false
  }
  def apply(s: String): List[RegexAtom] = regularExpression.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => result

    case Failure(msg, _)    => parseFailure(msg, s)

    case Error(msg, _)      => parseError(msg, s)
  }

  private def parseFailure(msg: String, s: String) = error("The pattern " + this.toString + " does not match " + s + ": " + msg)

  private def parseError(msg: String, s: String)   = error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)
}