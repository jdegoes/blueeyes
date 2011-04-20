package blueeyes.core.http

import scala.util.parsing.combinator._

case class URI(scheme: Option[String], userInfo: Option[String], host: Option[String], port: Option[Int], path: Option[String], query: Option[String], fragment: Option[String]){
  private lazy  val _toString: String = List(scheme.map(_ + ":"), host.orElse(port).map(v =>"//" + authority.getOrElse("")).orElse(authority), path, query.map("?" + _), fragment.map("#" + _)).map(_.getOrElse("")).mkString("")

  lazy val authority  = List(userInfo.map(_ + "@"), host, port.map(":" + _)).filter(_ != None) match{
    case x :: xs => Some((x :: xs).map(_.getOrElse("")).mkString(""))
    case Nil => None
  }
  val isAbsolute = scheme.map(v => true).getOrElse(false)

  override def toString = _toString
}

trait URIGrammar extends RegexParsers{
  def schemeParser   = (regex("""([a-zA-Z])([a-zA-Z\+\d\.-]+)""".r) <~ ":")?

  def userInfoParser = (regex("""[^\:@]+(:[^\:@]+)?""".r) <~ "@")?

  def hostParser     = (regex("""([a-zA-Z\d-]+\.)*([a-zA-Z\d-]+)""".r))?

  def portParser     = (":" ~> (regex("""[\d]+""".r) ^^ {case v => v.toInt}) )?

  def authorityParser = (("//" ~> userInfoParser ~ hostParser ~ portParser) ^^ {case  userInfo ~ host ~ port => URI(None, userInfo, host, port, None, None, None)} | userInfoParser ^^ {case  userInfo => URI(None, userInfo, None, None, None, None, None)})?

  def pathParser     = (regex("""[^?#]+""".r))?

  def queryParser    = ("?" ~> regex("""[^#]+""".r))?

  def fragmentParser = ("#" ~> regex(""".+""".r))?

  def parser = schemeParser ~ authorityParser ~ pathParser ~ queryParser ~ fragmentParser ^^ {case scheme ~ authority ~ path ~ query ~ fragment => URI(scheme, authority.flatMap(_.userInfo), authority.flatMap(_.host), authority.flatMap(_.port), path, query, fragment)}
}

import scala.util.parsing.input._
object URI extends URIGrammar{
  def apply(s: String): URI = parser.apply(new CharSequenceReader(s.trim)) match {
    case Success(result, _) => result

    case Failure(msg, _)    => parseFailure(msg, s)

    case Error(msg, _)      => parseError(msg, s)
  }

  def opt(s: String) = {
    try {
      val uri = apply(s)
      Some(uri)
    }
    catch {
      case _ => None
    }
  }

  private def parseFailure(msg: String, s: String) = error("The pattern " + this.toString + " does not match " + s + ": " + msg)

  private def parseError(msg: String, s: String)   = error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)

  def parseEmails(s: String): Option[URI] = {
    def emailParser = (regex("""([a-zA-Z\d_-]|\.)+@([a-zA-Z\d_-]|\.)+""".r)?) ^^ {case email => email.map(v => URI(v))}

    emailParser(new CharSequenceReader(s.trim)) match {
      case Success(result, _) => result

      case Failure(msg, _) => parseFailure(msg, s)

      case Error(msg, _) => parseFailure(msg, s)
    }
  }
}