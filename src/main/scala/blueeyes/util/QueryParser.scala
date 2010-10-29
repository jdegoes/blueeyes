package blueeyes.util

/**
 * This object only deals with the query portion of the URI.  A URI can be decomposed into
 * constituant components using the java.lang.URI class.
 * Note:  The individual parameters/values are assumed to be encoded, if neccesary
 */
object QueryParser {
  val encoding = "UTF-8"

  def parseQuery(query: String): Map[Symbol, String] = Map((query.split("&").toList.flatMap { nameEqualsValue =>
    nameEqualsValue.split("=").toList match {
      case "" :: Nil => Nil
      case name :: Nil => (name, "") :: Nil
      case name :: value :: Nil => (name, value) :: Nil
      case name :: everythingElse => (name, everythingElse.mkString("=")) :: Nil
      case Nil => Nil
    }
  }).map { nameValue =>
    import java.net.URLDecoder._
    (Symbol(decode(nameValue._1, encoding)), decode(nameValue._2, encoding))
  }: _*)

  def unparseQuery(query: Map[Symbol, String]): String = query.map { nameValue =>
    import java.net.URLEncoder._
    (encode(nameValue._1.name, encoding) :: encode(nameValue._2, encoding) :: Nil).filter(n => 
      !n.isEmpty).mkString("=")}.mkString("&")
}
