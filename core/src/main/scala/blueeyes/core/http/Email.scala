package blueeyes.core.http

import util.parsing.combinator._
import util.parsing.input.CharSequenceReader

case class Email(email: String)

object Emails extends RegexParsers{
  def apply(s: String): Option[Email] = {
    def emailParser = (regex("""([a-zA-Z\d_-]|\.)+@([a-zA-Z\d_-]|\.)+""".r)?) ^^ {case email => email.map(v => Email(v))}

    emailParser(new CharSequenceReader(s.trim)) match {
      case Success(result, _) => result

      case Failure(msg, _) => None

      case Error(msg, _) => None
    }
  }
}