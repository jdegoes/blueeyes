package blueeyes.core.http

/* For use in the Content Range Header */

sealed trait ContentByteRange {
  
  def unit: String        // Usually "bytes 

  def bytePair: BytePair  // From the ByteRange class

  def instanceLength: String  // either a number or *

  def value = unit + " " + bytePair.toString + "/" + instanceLength

  def ToString = value
}

object ContentByteRanges {

  def parseContentByteRanges(inString: String): Option[ContentByteRange] = {
    def unit: String =  """([a-z]+)""".r.findFirstIn(inString.toLowerCase).getOrElse("byte")
    def pair: Array[Int] = """\d+-\d+""".r.findFirstIn(inString).getOrElse("").split("-").map(_.toInt)
    if (pair.length != 2) 
      return None
    def bpair: BytePair = new BytePair(Some(pair(0)), pair(1))
    def length: String = """/\d+|/*""".r.findFirstIn(inString).getOrElse("")

    return Some(ByteInstance (unit, bpair, length))
  }

  case class ByteInstance (unit: String, bytePair: BytePair, instanceLength: String) extends ContentByteRange

}
