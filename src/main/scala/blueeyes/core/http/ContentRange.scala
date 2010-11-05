package blueeyes.core.http

/* For use in the Content Range Header */

sealed trait ContentByteRange {
  
  def unit: String        // Usually "bytes 

  def bytePair: ByteRanges.BytePair  // From the ByteRange class

  def instanceLength: String  // either a number or *

  def value = unit + "=" + bytePair.toString + "/" + instanceLength

  override def toString = value
}

object ContentByteRanges {

  def parseContentByteRanges(inString: String): Option[ContentByteRange] = {
    def unit: String =  """([a-z]+)""".r.findFirstIn(inString.toLowerCase).getOrElse("none")
    if (unit == "none") 
      return None

    def pair: Array[Int] = """\d+-\d+""".r.findFirstIn(inString).getOrElse("").split("-").map(_.toInt)
    if (pair.length != 2) 
      return None

    def bpair: ByteRanges.BytePair = new ByteRanges.BytePair(
      Some(HttpNumbers.LongNumber(pair(0))), HttpNumbers.LongNumber(pair(1)))

    def length: String = """(?<=/)(\d+)|\*""".r.findFirstIn(inString).getOrElse("none")
    if (length == "none") 
      return None 

    return Some(ByteInstance (unit, bpair, length))
  }

  case class ByteInstance (unit: String, bytePair: ByteRanges.BytePair, instanceLength: String) extends ContentByteRange

}
