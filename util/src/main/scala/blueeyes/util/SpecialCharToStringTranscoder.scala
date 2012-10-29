package blueeyes.util

/** Transcodes special characters to characters.
 */
case class SpecialCharToStringTranscoder(encoding: PartialFunction[Char, String], decoding: PartialFunction[List[Char], Option[Char]]) {

  private val encodingF: Char => Option[String] = encoding.lift

  private val decodingF: List[Char] => Option[Option[Char]] = decoding.lift

  /** Takes an decoded string, and encodes it.
   */
  def encode(s: String): String = {
    val encoded = new StringBuilder

    for (i <- 0 until s.length) {
      val c = s.charAt(i)

      encodingF(c) match {
        case Some(remapped) =>
          encoded.append(remapped)

        case None =>
          encoded.append(c)
      }
    }

    encoded.toString
  }

  /** Takes an encoded string, and decodes it.
   */
  def decode(s: String): String = {
    val decoded     = new StringBuilder
    var decodingPart: List[Char] = Nil

    for (i <- 0 until s.length) {
      decodingPart = decodingPart ::: List(s.charAt(i))

      decodingF(decodingPart) match {
        case Some(None) =>
        case Some(Some(decodedChar)) =>
          decoded.append(decodedChar)
          decodingPart = Nil
        case None =>
          decoded.append(decodingPart.mkString(""))
          decodingPart = Nil
      }
    }
    decoded.append(decodingPart.mkString(""))

    decoded.toString
  }
}
