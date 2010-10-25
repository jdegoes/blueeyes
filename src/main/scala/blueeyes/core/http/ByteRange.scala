package blueeyes.core.http

sealed trait ByteRange {
  def unit: String 
  def bytePairs: List[BytePair]
  def value: String = unit + "=" + bytePairs.map(_.toString).mkString(",")
  override def toString = value

}

object ByteRanges {

  def parseByteRange(inString: String): ByteRange = {
    def splitString = inString.toLowerCase.trim.split("=");
    if (splitString.length % 2 == 1)
      splitString ++ "" 
    def brb = new ByteRangeBuilder(splitString(0))
    def bytes = splitString(1).trim.split(",").toList.map(_.split("-")).map(x => x match {
      case Array(a) => brb.addBytePair(a.toInt)
      case Array(a, b) => brb.addBytePair(a.toInt, b.toInt)
    })
    brb.constructByteRange 
  }

  case class ByteRangeList(bytePairs: List[BytePair], unit: String) extends ByteRange

  sealed class ByteRangeBuilder(unit: String) {
    var pairs: List[BytePair] = List()
    var name: String = unit

    def addBytePair(first: Int, last: Int) = BytePair(Some(first), last) :: pairs
    def addBytePair(last: Int) = BytePair(None, last) :: pairs
    def constructByteRange: ByteRange = ByteRangeList(pairs, name)
  }

  case class NullByteRange(bytePairs: List[BytePair], unit: String) extends ByteRange {
    override def value = ""
  }
}

sealed case class BytePair(first: Option[Int], last: Int) {
  override def toString = first.getOrElse("") + "-" + last
}
