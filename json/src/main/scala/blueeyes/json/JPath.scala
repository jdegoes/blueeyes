package blueeyes.json

import util.matching.Regex

import JsonAST._

sealed trait JPath { self =>
  def nodes: List[JPathNode]

  def parent: Option[JPath] = if (nodes.isEmpty) None else Some(JPath(nodes.take(nodes.length - 1): _*))

  def ancestors: List[JPath] = {
    def ancestors0(path: JPath, acc: List[JPath]): List[JPath] = {
      path.parent match {
        case None => acc

        case Some(parent) => ancestors0(parent, parent :: acc)
      }
    }

    ancestors0(this, Nil).reverse
  }

  def \ (that: JPath):  JPath = JPath(self.nodes ++ that.nodes)
  def \ (that: String): JPath = JPath(self.nodes :+ JPathField(that))
  def \ (that: Int):    JPath = JPath(self.nodes :+ JPathIndex(that))

  def apply(index: Int): JPathNode = nodes(index)

  def extract(jvalue: JValue): JValue = {
    def extract0(path: List[JPathNode], d: JValue): JValue = path match {
      case Nil => d

      case head :: tail => head match {
        case JPathField(name)  => extract0(tail, d \ name)
        case JPathIndex(index) => extract0(tail, d(index))
      }
    }

    extract0(nodes, jvalue)
  }

  def head: Option[JPathNode] = nodes.headOption

  def tail: JPath = JPath(nodes.tail: _*)

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

  def iterator = nodes.iterator

  def length = nodes.length

  override def toString = path
}

sealed trait JPathNode
object JPathNode {
  implicit def s2PathNode(name: String): JPathNode = JPathField(name)
  implicit def i2PathNode(index: Int): JPathNode = JPathIndex(index)
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

  implicit def apply(path: String): JPath = {
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

  implicit def singleNodePath(node: JPathNode) = JPath(node)

  implicit val OrderingJValue = new Ordering[JPath] {
    def compare(v1: JPath, v2: JPath): Int = v1.toString.compare(v2.toString)
  }
}

trait JPathImplicits {
  implicit def stringToJPath(s: String): JPath = JPath(s)
}
object JPathImplicits extends JPathImplicits
