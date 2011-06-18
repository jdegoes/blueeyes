package blueeyes.util

/** Transcodes special characters using an escape character.
 */
case class SpecialCharTranscoder(val escape: Char, encoding: PartialFunction[Char, Char], decoding: PartialFunction[Char, Char]) {
  private val defaultEscapeMapping: PartialFunction[Char, Char] = {
    case `escape` => escape
  }
  
  private val encodingF: Char => Option[Char] = encoding.orElse(defaultEscapeMapping).lift
  
  private val decodingF: Char => Option[Char] = decoding.orElse(defaultEscapeMapping).lift
  
  /** Takes an decoded string, and encodes it.
   */
  def encode(s: String): String = {
    val encoded = new StringBuilder
    
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      
      encodingF(c) match {
        case Some(remapped) =>
          encoded.append(escape).append(remapped)
          
        case None =>
          encoded.append(c)
      }
    }
    
    encoded.toString
  }
  
  /** Takes an encoded string, and decodes it.
   */
  def decode(s: String): String = {
    val decoded = new StringBuilder
    var escaping = false
    
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      
      if (escaping) {
        val original = decodingF(c).getOrElse(sys.error("Expected to find encoded character but found: " + c))
        
        decoded.append(original)
        
        escaping = false
      }
      else c match {
        case `escape` => escaping = true
        case _ => decoded.append(c)
      }
    }
    
    decoded.toString
  }
}
object SpecialCharTranscoder {
  def fromMap(escape: Char, map: Map[Char, Char]) = new SpecialCharTranscoder(escape, map, map.map(t => (t._2, t._1)))
}