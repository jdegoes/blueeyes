package blueeyes.core.http

/* For use with the Range Header */

sealed trait ByteRange {

  def unit: String 
  def bytePairs: List[ByteRanges.BytePair]
  def value: String = unit + "=" + bytePairs.map(_.toString).mkString(", ")
  override def toString = value

}

object ByteRanges {

  def parseByteRanges(inString: String): Option[ByteRange] = {
    def unit = """[a-zA-Z]+""".r.findFirstIn(inString.trim).getOrElse("")
    if (unit == "") 
      return None

    def BytesRegex = """([\d\s]|,|-)+""".r
    def byteString: List[String] = BytesRegex.findFirstIn(inString.trim).getOrElse("")
      .split(",").toList.map(_.trim)

    def bytes: List[BytePair] = byteString.map(_.split("-")).map(numPair => 
        numPair.map(x => HttpNumbers.parseHttpNumbers(x)) 
        match {
          case Array(None, Some(y))     => Some(BytePair(None, y))
          case Array(Some(x), Some(y))  => Some(BytePair(Some(x), y))
          case default                  => None
        }
      ).filterNot(x => x == None).map(_.get)
    if (bytes.length == 0) 
      return None
    return Some(ByteRangeList(bytes, unit))
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

