package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import blueeyes.core.http.HttpNumbers.LongNumber

/* For use with the Range Header */

sealed trait ByteRange {

  def unit: String 
  def bytePairs: List[ByteRanges.BytePair]
  def value: String = unit + "=" + bytePairs.map(_.toString).mkString(", ")
  override def toString = value

}

object ByteRanges extends RegexParsers with HttpNumberImplicits{

  private def digitalParser = regex("""[\d]+""".r)

  private def bytePairParser = opt(
    digitalParser ~ ("-" ~> digitalParser) ^^ {case first ~ last => BytePair(Some(LongNumber(first.toLong)), LongNumber(last.toLong))} |
    "-" ~> digitalParser ^^ {case last => BytePair(None, LongNumber(last.toLong))}
  )
  private def bytePairsParser = repsep(bytePairParser, regex("""[ ]*,[ ]*""".r)) ^^ {case values => values.filter(_ != None).map(_.get) }

  private def parser = opt(regex("""[a-zA-Z]+""".r) <~ "=") ~ bytePairsParser ^^ {case unit ~ pairs => unit.flatMap(unitValue => pairs match {
    case x :: xs => Some(ByteRangeList(pairs, unitValue))
    case Nil => None
  })}

  def parseByteRanges(inString: String) = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => error("The ByteRanges " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case class ByteRangeList(bytePairs: List[BytePair], unit: String) extends ByteRange

  sealed class ByteRangeBuilder(unit: String) {
    var pairs: List[BytePair] = List()
    var name: String = unit

    def addBytePair(first: HttpNumber, last: HttpNumber) = BytePair(Some(first), last) :: pairs
    def addBytePair(last: HttpNumber) = BytePair(None, last) :: pairs

    def constructByteRange: ByteRange = ByteRangeList(pairs, name)

  }

  case class BytePair(first: Option[HttpNumber], last: HttpNumber) {
    override def toString = first.map(_.value).getOrElse("") + "-" + last.value
  }
  object BytePair {
    def apply(first: HttpNumber, last: HttpNumber): BytePair = new BytePair(Some(first), last)
    def apply(last: HttpNumber): BytePair = new BytePair(None, last)
  }

  case class NullByteRange(bytePairs: List[BytePair], unit: String) extends ByteRange {
    override def value = ""
  }
}

