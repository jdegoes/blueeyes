package blueeyes.core.http

import scala.util.parsing.combinator._

case class URI(scheme: Option[String] = None, userInfo: Option[String] = None, host: Option[String] = None, port: Option[Int] = None, path: Option[String] = None, query: Option[String] = None, fragment: Option[String] = None){ self =>
  import URITranscoders._
  private lazy  val _toString: String = List(scheme.map(_ + ":"), host.orElse(port).map(v =>"//" + authority.getOrElse("")).orElse(authority), path, query.map("?" + _), fragment.map("#" + _)).map(_.getOrElse("")).mkString("")

  lazy val authority  = List(userInfo.map(_ + "@"), host, port.map(":" + _)).filter(_ != None) match{
    case x :: xs => Some((x :: xs).map(_.getOrElse("")).mkString(""))
    case Nil => None
  }

  val isAbsolute = scheme.map(v => true).getOrElse(false)

  def encode = self.copy(path = path.map(pathTranscoder.encode(_)), query = query.map(queryTranscoder.encode(_)))
  def decode = self.copy(path = path.map(pathTranscoder.decode(_)), query = query.map(queryTranscoder.decode(_)))

  override def toString = _toString
}

trait URIGrammar extends RegexParsers {
  def schemeParser   = (regex("""([a-zA-Z])([a-zA-Z\+\d\.-]+)""".r) <~ ":")?

  def userInfoParser = (regex("""[^\]:/?#\[@]+(:[^\]:/?#\[@]+)""".r) <~ "@")?

  def hostParser     = (regex("""([a-zA-Z\d-]+\.)*([a-zA-Z\d-]+)""".r))?  // doesn't support IPv6

  def portParser     = (":" ~> (regex("""[\d]+""".r) ^^ {case v => v.toInt}) )?

  def authorityParser1 = ("//" ~> userInfoParser ~ hostParser ~ portParser) ^^ {
    case userInfo0 ~ host0 ~ port0 => URI(userInfo = userInfo0, host = host0, port = port0)
  }

  def authorityParser2 = userInfoParser ^^ {
    case userInfo0 => URI(userInfo = userInfo0)
  }

  def authorityParser =  (authorityParser1 | authorityParser2)?

  def pathParser     = (regex("""[^?#\[\]]+""".r))?

  def queryParser    = ("?" ~> regex("""[^#\[\]]+""".r))?

  def fragmentParser = ("#" ~> regex(""".+""".r))?

  def parser = schemeParser ~ authorityParser ~ pathParser ~ queryParser ~ fragmentParser ^^ {
    case scheme ~ authority ~ path ~ query ~ fragment => 
      URI(scheme, authority.flatMap(_.userInfo), authority.flatMap(_.host), authority.flatMap(_.port), path, query, fragment)
  }
}

import scala.util.parsing.input._
object URI extends URIGrammar {
  implicit def stringToUri(uri: String) = URI(uri)

  def apply(s: String): URI = {
    parser.apply(new CharSequenceReader(s.trim)) match {
      case Success(result, _) => result

      case Failure(msg, _)    => parseFailure(msg, s)

      case Error(msg, _)      => parseError(msg, s)
    }
  }

  def opt(s: String) = {
    try {
      Some(apply(s))
    } catch {
      case _ => None
    }
  }

  private def parseFailure(msg: String, s: String) = sys.error("The pattern " + this.toString + " does not match " + s + ": " + msg)

  private def parseError(msg: String, s: String)   = sys.error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)
}
