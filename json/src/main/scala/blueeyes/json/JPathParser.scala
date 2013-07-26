package blueeyes.json

import scala.util.parsing.combinator._
import scalaz.\/
import scalaz.syntax.id._


object JPathParser extends RegexParsers {

  def parse(in: String): String \/ List[JPathNode] =
    this.parseAll(jPath, in) match {
      case Success(result, _) => result.right
      case failure: NoSuccess => failure.msg.left
    }

  def identifier: Parser[String] =
    "[a-zA-Z_$][a-zA-Z_$0-9]*".r


  def jPath: Parser[List[JPathNode]] = {
    def elements: Parser[List[JPathNode]] =
      ((identifier ^^ { JPathField(_) }) | pathElement) ~ (pathElement.*) ^^ {
        case n ~ ns => n +: ns
      }

    identity | elements
  }


  def identity: Parser[List[JPathNode]] =
    """^\s*\.?\s*$""".r ^^ { _ => List.empty[JPathNode] }


  def pathElement: Parser[JPathNode] =
    field | index


  def field: Parser[JPathField] =
    "." ~ identifier ^^ { case dot ~ id => JPathField(id) }


  def index: Parser[JPathIndex] =
    sqIndexField | dqIndexField | numericIndexField



  def numericIndexField: Parser[JPathIndex] = {
    def index: Parser[Int] =
      """\d+""".r ^^ { _.toInt }

    "[" ~ index ~ "]" ^^ { case lb ~ id ~ rb => JPathIndex(id) }
  }


  def dqIndexField: Parser[JPathIndex] = {
    def unquotedString: Parser[String] =
      """[^"\\]+""".r

    def quotedString: Parser[String] =
      """\\.""".r ^^ { _.replace("\\", "") }

    def index: Parser[String] =
      ( unquotedString | quotedString ).* ^^ { _.mkString }

    "[\"" ~ index ~ "\"]" ^^ { case lq ~ id ~ rq => JPathIndex(id) }
  }


  def sqIndexField: Parser[JPathIndex] = {
    def unquotedString: Parser[String] =
      """[^'\\]+""".r

    def quotedString: Parser[String] =
      """\\.""".r ^^ { _.replace("\\", "") }

    def index: Parser[String] =
      ( unquotedString | quotedString ).* ^^ { _.mkString }

    "['" ~ index ~ "']" ^^ { case lq ~ id ~ rq => JPathIndex(id) }
  }
}
