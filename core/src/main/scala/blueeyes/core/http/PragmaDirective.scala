package blueeyes.core.http
import blueeyes.util.ProductPrefixUnmangler

sealed trait PragmaDirective extends ProductPrefixUnmangler{
  def value: String = productPrefix

  override def toString = value
}

object PragmaDirectives {

  def parsePragmaDirectives(inString: String): Option[PragmaDirective] = {
    inString.trim.toLowerCase match {
      case "no-cache" => Some(`no-cache`)
      case any => None
    }
  }

  case object `no-cache` extends PragmaDirective

  sealed case class CustomPragmaDirective(override val value: String) extends PragmaDirective

}
