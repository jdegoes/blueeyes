package blueeyes.json

import util.matching.Regex

import JsonAST._

sealed trait JPath extends Function1[JValue, JValue] { self =>
  def nodes: List[JPathNode]
  
  def \ (that: JPath): JPath = new JPath {
    def nodes = self.nodes ++ that.nodes
  }
  
  def \ (that: String): JPath = this \ JPath(that)
  
  def apply(name: String): JPath = this \ JPathField(name)
  
  def apply(index: Int): JPath = this \ JPathIndex(index)
  
  def apply(jvalue: JValue): JValue = extract(jvalue)
  
  def extract(jvalue: JValue): JValue = {
    def extract0(path: List[JPathNode], d: JValue): List[JValue] = path match {
      case Nil => d :: Nil
      
      case head :: tail => head match {
        case JPathField(name)  => extract0(tail, jvalue \ name)
        case JPathIndex(index) => extract0(tail, jvalue(index))
        case JPathRegex(regex) => jvalue.children.flatMap { child => 
          child match {
            case JField(regex(name), value) => extract0(tail, value)
            
            case _ => Nil
          }
        }
      }
    }
  
    extract0(nodes, jvalue) match {
      case Nil => JNothing
      case x :: Nil => x
      case xs => JArray(xs)
    }
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
sealed case class JPathRegex(regex: Regex) extends JPathNode {
  override def toString = "/" + regex.pattern.toString + "/"
  
  override def hashCode = toString.hashCode
  
  override def equals(that: Any): Boolean = that match {
    case x: JPathRegex => this.toString == that.toString
    
    case _ => false
  }
}

object JPath {
  private val IndexPattern = """^\[(\d+)\]$""".r
  private val WildPattern  = """^([*])$""".r
  private val RegexPattern = """^/(.+)/([ixm]*)$""".r
  
  private val NodePatterns = """\.|(?=\[\d+\])""".r
  
  
  def apply(n: JPathNode*) = new JPath {
    lazy val nodes = n.toList
  }
  
  def apply(path: String): JPath = {
    def parse0(segments: List[String]): List[JPathNode] = segments match {
      case Nil => Nil
      
      case head :: tail =>
        (head match {
          case IndexPattern(index) => JPathIndex(index.toInt)
          
          case WildPattern(star) => JPathRegex(".*".r)
          
          case RegexPattern(regex, flags)  => 
            val prefix = Map("i" -> "(?i)", "x" -> "(?x)", "m" -> "(?m)").foldLeft("") { (str, rep) => (if (flags.contains(rep._1)) rep._2 else "") + str }
            
            JPathRegex(new Regex(prefix + regex))
            
          case name => JPathField(name)
        }) :: parse0(tail)
    }
  
    apply(parse0(NodePatterns.split(path).toList.filter(_.length > 0)): _*)
  }
}

trait JPathImplicits {
  implicit def stringToJPath(s: String): JPath = JPath(s)
}
object JPathImplicits extends JPathImplicits