package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex

/* From :
  IANA: 
  http://www.iana.org/assignments/character-sets
*/

sealed trait CharSet {
  def charName: String
  def value: String = charName
  def aliases: List[String] 
}

object CharSets {

  def parseCharSets(inString: String): Array[CharSet] = {
    def CharSetRegex = new Regex("""([a-zA-Z]\-_)+""")

    var outCharSets: Array[CharSet] = inString.split(",").map(_.trim)
        .flatMap(CharSetRegex findFirstIn _)
        .map ( charSet =>  charSet match {
            case "US-ASCII" => `US-ASCII`
            case "ISO-8859-1" => `ISO-8859-1`
            case "ISO-8859-2" => `ISO-8859-2`
            case "ISO-8859-3" => `ISO-8859-3`
            case "ISO-8859-4" => `ISO-8859-4`
            case "ISO-8859-5" =>  `ISO-8859-5`
            case "ISO-8859-6" =>  `ISO-8859-6`
            case "ISO-8859-7" =>  `ISO-8859-7`
            case "ISO-8859-8" =>  `ISO-8859-8`
            case "ISO-8859-9" =>  `ISO-8859-9`
            case "ISO-8859-10" =>  `ISO-8859-10`
            case _ => new CustomCharSet(charSet)
          }
        )
    return outCharSets
  }

  trait GenericCharSet extends ProductPrefixUnmangler with CharSet{
    def charName = unmangledName 
  }

  sealed abstract class StandardCharSet(val aliases: List[String]) extends GenericCharSet 
  
  /* Probably we should include some standard unicode fonts as well */

  case object `US-ASCII` extends StandardCharSet("iso-ir-6" :: "ANSI_X3.4-1986" :: "ISO_646.irv:1991" :: "ASCII" :: "ISO646-US" :: "US-ASCII" :: "us" :: "IBM367" :: "cp367" :: "csASCII" :: Nil)

  case object `ISO-8859-1` extends StandardCharSet("iso-ir-100"::"ISO_8859-1" :: "ISO-8859-1" :: "latin1" :: "l1" :: "IBM819" :: "CP819" :: "csISOLatin1" :: Nil)

  case object `ISO-8859-2` extends StandardCharSet("iso-ir-101" :: "ISO_8859-2" :: "ISO-8859-2" :: "latin2" :: "l2" :: "csISOLatin2" :: Nil) 

  case object `ISO-8859-3` extends StandardCharSet("iso-ir-109" :: "ISO_8859-3" :: "ISO-8859-3" :: "latin3" :: "l3" :: "csISOLatin3" :: Nil)

  case object `ISO-8859-4` extends StandardCharSet("iso-ir-110" :: "ISO_8859-4" :: "ISO-8859-4" :: "latin4" :: "l4" :: "csISOLatin4" :: Nil)

  case object `ISO-8859-5` extends StandardCharSet("iso-ir-144" :: "ISO_8859-5" :: "ISO-8859-5" :: "cyrillic" :: "csISOLatinCyrillic" :: Nil)

  case object `ISO-8859-6` extends StandardCharSet("iso-ir-127" :: "ISO_8859-6" :: "ISO-8859-6" :: "ECMA-114" :: "ASMO-708" :: "arabic" :: "csISOLatinArabic" :: Nil)

  case object `ISO-8859-7` extends StandardCharSet("iso-ir-126" :: "ISO_8859-7" :: "ISO-8859-7" :: "ELOT_928" :: "ECMA-118" :: "greek" :: "greek8" :: "csISOLatinGreek" :: Nil)

  case object `ISO-8859-8` extends StandardCharSet("iso-ir-138" :: "ISO_8859-8" :: "ISO-8859-8" :: "hebrew" :: "csISOLatinHebrew" :: Nil)

  case object `ISO-8859-9` extends StandardCharSet("iso-ir-148" :: "ISO_8859-9" :: "ISO-8859-9" :: "latin5" :: "l5" :: "csISOLatin5" :: Nil)

  case object `ISO-8859-10` extends StandardCharSet("iso-ir-157" :: "l6" :: "ISO_8859-10" :: "csISOLatin6" :: "latin6" :: Nil)

  sealed case class CustomCharSet(override val value: String) extends StandardCharSet(value :: Nil) {
    override def toString = value;
  }

}

