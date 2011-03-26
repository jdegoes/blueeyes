package blueeyes.json

import util.matching.Regex

import JsonAST._

sealed trait JPath extends Function1[JValue, List[JValue]] { self =>
  def nodes: List[JPathNode]

  def parent: Option[JPath] = if (nodes.length == 0) None else Some(JPath(nodes.take(nodes.length - 1): _*))

  def ancestors: List[JPath] = {
    def ancestors0(path: JPath, acc: List[JPath]): List[JPath] = {
      path.parent match {
        case None => acc

        case Some(parent) => ancestors0(parent, parent :: acc)
      }
    }

    ancestors0(this, Nil).reverse
  }

  def \ (that: JPath): JPath = JPath(self.nodes ++ that.nodes)

  def \ (that: String): JPath = this \ JPath(that)

  def apply(name: String): JPath = this \ JPathField(name)

  def apply(index: Int): JPath = this \ JPathIndex(index)

  def apply(jvalue: JValue): List[JValue] = extract(jvalue)

  def extract(jvalue: JValue): List[JValue] = {
    def extract0(path: List[JPathNode], d: JValue): JValue = path match {
      case Nil => d

      case head :: tail => head match {
        case JPathField(name)  => extract0(tail, d \ name)
        case JPathIndex(index) => extract0(tail, jvalue(index))
      }
    }

    expand(jvalue).map { jpath =>
      extract0(jpath.nodes, jvalue)
    }
  }

  def expand(jvalue: JValue): List[JPath] = {
    def isRegex(s: String) = s.startsWith("(") && s.endsWith(")")

    def expand0(current: List[JPathNode], right: List[JPathNode], d: JValue): List[JPath] = right match {
      case Nil => JPath(current) :: Nil

      case head :: tail => head match {
        case x @ JPathIndex(index) => expand0(current :+ x, tail, jvalue(index))
        case x @ JPathField(name) if (isRegex(name)) => {
          val regex = name.r

          jvalue.children.flatMap { child =>
            child match {
              case JField(regex(name), value) =>
                val expandedNode = JPathField(name)

                expand0(current :+ expandedNode, tail, value)

              case _ => Nil
            }
          }
        }
        case x @ JPathField(name) => expand0(current :+ x, tail, jvalue \ name)
      }
    }

    expand0(Nil, nodes, jvalue)
  }

  def path = nodes.mkString("")

  override def toString = path
}

sealed trait JPathNode extends JPath { self =>
  def nodes = this :: Nil
}
sealed case class JPathField(name: String) extends JPathNode {
  override def toString = "." + name
}
sealed case class JPathIndex(index: Int) extends JPathNode {
  override def toString = "[" + index + "]"
}

object JPath {
  private[this] case class CompositeJPath(nodes: List[JPathNode]) extends JPath

  private val PathPattern  = """\.|(?=\[\d+\])""".r
  private val IndexPattern = """^\[(\d+)\]$""".r

  val Identity = apply()

  def apply(n: JPathNode*): JPath = CompositeJPath(n.toList)

  def apply(l: List[JPathNode]): JPath = apply(l: _*)

  def unapplySeq(path: String): Option[List[JPathNode]] = Some(apply(path).nodes)

  def apply(path: String): JPath = {
    def parse0(segments: List[String], acc: List[JPathNode]): List[JPathNode] = segments match {
      case Nil => acc

      case head :: tail =>
        if (head.trim.length == 0) parse0(tail, acc)
        else parse0(tail,
          (head match {
            case IndexPattern(index) => JPathIndex(index.toInt)

            case name => JPathField(name)
          }) :: acc
        )
    }

    val properPath = if (path.startsWith(".")) path else "." + path

    apply(parse0(PathPattern.split(properPath).toList, Nil).reverse: _*)
  }
}

trait JPathImplicits {
  implicit def stringToJPath(s: String): JPath = JPath(s)
}
object JPathImplicits extends JPathImplicits