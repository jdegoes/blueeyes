package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._
/* For use in the Content Range Header */

sealed trait ContentByteRange {
  
  def unit: String        // Usually "bytes 

  def bytePair: ByteRanges.BytePair  // From the ByteRange class

  def instanceLength: String  // either a number or *

  def value = unit + "=" + bytePair.toString + "/" + instanceLength

  override def toString = value
}

object ContentByteRanges extends RegexParsers{

  private def digitalParser = regex("""[\d]+""".r)

  private def bytePairParser = (digitalParser <~ "-") ~ digitalParser ^^ {case first ~ last => new ByteRanges.BytePair(Some(HttpNumbers.LongNumber(first.toInt)), HttpNumbers.LongNumber(last.toInt))}

  private def parser = opt(
    regex("[a-z]+".r) ~ bytePairParser ~ ("/" ~> regex("""(\d+)|\*""".r)) ^^ {case unit ~ bpair ~ length => ByteInstance (unit, bpair, length)}
  )

  def parseContentByteRanges(inString: String) = parser(new CharSequenceReader(inString.toLowerCase)) match {
    case Success(result, _) => result

    case Failure(msg, _) => error("The ContentByteRanges " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case class ByteInstance (unit: String, bytePair: ByteRanges.BytePair, instanceLength: String) extends ContentByteRange

}
