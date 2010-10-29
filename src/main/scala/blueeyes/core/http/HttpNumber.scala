package blueeyes.core.http


/* A method to ensure that numbers are dealt with as we would hope in Http
 * Headers -- i.e., paser should return an Option[Long] and not die on mangled
 * cases */
sealed trait HttpNumber {

  def number: Long
  def value = number.toString
  override def toString = value
}

object HttpNumbers {

  def parseHttpNumbers(inString: String): Option[HttpNumber] = {
    def NumParser = """[\d]+""".r
    return NumParser.findFirstIn(inString.trim).map(x => LongNumber(x.toLong))
  }

  case class LongNumber(number: Long) extends HttpNumber

}


trait HttpNumberImplicits { 
  
  implicit def int2HttpNumber(num: Int): HttpNumber = {
    HttpNumbers.LongNumber(num)
  }

  implicit def long2HttpNumber(long: Long): HttpNumber = {
    HttpNumbers.LongNumber(long)
  }

}

object HttpNumberImplicits extends HttpNumberImplicits {
    import blueeyes.core.http.HttpNumbers
    import blueeyes.core.http.HttpNumber
}





