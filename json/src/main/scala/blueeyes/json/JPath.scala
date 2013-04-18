package blueeyes.json

import util.matching.Regex

import serialization._
import serialization.DefaultSerialization._
import scalaz.Order
import scalaz.Ordering
import scalaz.Ordering._

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

  def \: (that: JPath):  JPath = JPath(that.nodes ++ self.nodes)
  def \: (that: String): JPath = JPath(JPathField(that) +: self.nodes)
  def \: (that: Int):    JPath = JPath(JPathIndex(that) +: self.nodes)

  def hasPrefix(p: JPath): Boolean = nodes.startsWith(p.nodes)

  def dropPrefix(p: JPath): Option[JPath] = {
    def remainder(nodes: List[JPathNode], toDrop: List[JPathNode]): Option[JPath] = {
      nodes match {
        case x :: xs =>
          toDrop match {
            case `x` :: ys => remainder(xs, ys)
            case Nil => Some(JPath(nodes))
            case _ => None
          }

        case Nil => 
          if (toDrop.isEmpty) Some(JPath(nodes)) 
          else None
      }
    }

    remainder(nodes, p.nodes)
  }

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
    def expand0(current: List[JPathNode], right: List[JPathNode], d: JValue): List[JPath] = right match {
      case Nil => JPath(current) :: Nil

      case head :: tail => head match {
        case x @ JPathIndex(index) => expand0(current :+ x, tail, jvalue(index))

        case x @ JPathField(name) => expand0(current :+ x, tail, jvalue \ name)
      }
    }

    expand0(Nil, nodes, jvalue)
  }

  def path = nodes.map(_.render).mkString("")

  def iterator = nodes.iterator

  def length = nodes.length

  override def toString = if (nodes.isEmpty) "." else path
}

sealed trait JPathNode {
  def \(that: JPath) = JPath(this :: that.nodes)
  def \(that: JPathNode) = JPath(this :: that :: Nil)
  def render: String
}

object JPathNode {
  implicit def s2PathNode(name: String): JPathNode = JPathField(name)
  implicit def i2PathNode(index: Int): JPathNode = JPathIndex(index)

  implicit object JPathNodeOrder extends Order[JPathNode] {
    def order(n1: JPathNode, n2: JPathNode): Ordering = (n1, n2) match {
      case (JPathField(s1), JPathField(s2)) => Ordering.fromInt(s1.compare(s2))
      case (JPathField(_) , _             ) => GT
      case (JPathIndex(i1), JPathIndex(i2)) => Ordering.fromInt(i1.compare(i2))
      case (JPathIndex(_) , _             ) => LT
    }
  }

  implicit val JPathNodeOrdering = JPathNodeOrder.toScalaOrdering
}

sealed case class JPathField(name: String) extends JPathNode {
  def render = "." + name
  override def toString = render //FIXME
}

sealed case class JPathIndex(index: Int) extends JPathNode {
  def render = "[" + index + "]"
  override def toString = "[" + index + "]"
}

trait JPathSerialization {
  implicit val JPathDecomposer : Decomposer[JPath] = new Decomposer[JPath] {
    def decompose(jpath: JPath) : JValue = JString(jpath.toString)
  }

  implicit val JPathExtractor : Extractor[JPath] = new Extractor[JPath] {
    def validated(obj : JValue) : scalaz.Validation[Extractor.Error,JPath] =
      obj.validated[String].map(JPath(_))
  }
}

object JPath extends JPathSerialization {
  private[this] case class CompositeJPath(nodes: List[JPathNode]) extends JPath 

  private val PathPattern  = """\.|(?=\[\d+\])""".r
  private val IndexPattern = """^\[(\d+)\]$""".r

  val Identity = apply()

  def apply(n: JPathNode*): JPath = CompositeJPath(n.toList)

  def apply(l: List[JPathNode]): JPath = apply(l: _*)

  def unapplySeq(path: JPath): Option[List[JPathNode]] = Some(path.nodes)

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

  implicit object JPathOrder extends Order[JPath] {
    def order(v1: JPath, v2: JPath): Ordering = {
      def compare0(n1: List[JPathNode], n2: List[JPathNode]): Ordering = (n1, n2) match {
        case (Nil    , Nil)     => EQ
        case (Nil    , _  )     => LT
        case (_      , Nil)     => GT
        case (n1::ns1, n2::ns2) =>
          val ncomp = Order[JPathNode].order(n1, n2)
          if(ncomp != EQ) ncomp else compare0(ns1, ns2)
      }

      compare0(v1.nodes, v2.nodes)
    }
  }

  implicit val JPathOrdering = JPathOrder.toScalaOrdering
}

trait JPathImplicits {
  class StringExtensions(s: String) {
    def jpath = JPath(s)
  }

  implicit def stringExtensions(s: String) = new StringExtensions(s)
}

object JPathImplicits extends JPathImplicits
