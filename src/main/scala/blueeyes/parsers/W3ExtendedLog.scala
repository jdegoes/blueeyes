package blueeyes.parsers

import scala.util.parsing.combinator._
import W3ExtendedLogAST._
import scala.util.parsing.input.CharSequenceReader

object W3ExtendedLog extends W3ExtendedLogGrammar with RegexParsers{
  def apply(s: String): List[Directive] = directives.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => {
      result
    }

    case Failure(msg, _)    => parseFailure(msg, s)

    case Error(msg, _)      => parseError(msg, s)
  }

  private def parseFailure(msg: String, s: String) = sys.error("The pattern " + this.toString + " does not match " + s + ": " + msg)

  private def parseError(msg: String, s: String)   = sys.error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)
}
