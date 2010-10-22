package blueeyes.core.http

import scala.util.matching.Regex

sealed trait Email {
  def name: String
  def domain: String
  def value: String = name + "@" + domain.toList
  override def toString = value
}

object Emails {

  def parseEmails(inString: String): Email = {
    def EmailRegex = new Regex("""([a-z])+([\.\-_][a-z]*)*\@([a-z])+([\.\-_][a-z]*)*""")
    val outEmail: Email = EmailRegex.findFirstIn(inString.toLowerCase.trim).getOrElse("")
      .split("@") match {
      case Array(name, domain) => StandardEmail(name, domain)
      case _ => NullEmail()
    }
    return outEmail
  }

  case class StandardEmail (name: String, domain: String) extends Email
  case class NullEmail() extends Email {
    override def name = ""
    override def domain = ""
    override def value = ""
  }

}
