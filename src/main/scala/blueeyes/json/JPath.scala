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
  private val FieldPattern = """^\.(.+)$""".r

  private val NodePatterns = """\.|(?=\[\d+\])""".r

  def apply(n: JPathNode*) = new JPath {
    lazy val nodes = n.toList
  }

  private[this] sealed trait Index
  private[this] case class Start(index: Int) extends Index
  private[this] case class End(index: Int) extends Index

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
            
          case FieldPattern(name) => JPathField(name)
        }) :: parse0(tail)
    }
    
    val properPath = if (path.startsWith(".")) path else "." + path
  
    apply(parse0(parseToSegments(properPath)): _*)
  }
  
  private def parseToSegments(path: String): List[String] = {
    def boundariesFor1(regex: Regex): List[(Int, Int)] = regex.findAllIn(path).matchData.map(m => (m.start, m.end)).toList
    
    def boundariesFor2(start: Regex, end: Regex): List[(Int, Int)] = {
      def findIndices(regex: Regex): List[Int] = regex.findAllIn(path).matchData.map(m => m.start).toList
      
      def findPairs(list: List[Index]): List[(Start, End)] = list match {
        case Start(idx1) :: Start(idx2) :: rest => findPairs(Start(idx1) :: rest)

        case Start(idx1) :: End(idx2) :: End(idx3) :: rest => findPairs(Start(idx1) :: End(idx3) :: rest)

        case Start(idx1) :: End(idx2) :: rest => (Start(idx1), End(idx2)) :: findPairs(rest)

        case End(idx1) :: rest => findPairs(rest)

        case Start(idx1) :: Nil => Nil

        case Nil => Nil
      }

      val indices = findIndices(start).map(Start(_)) ++ findIndices(end).map(End(_))

      findPairs(indices).map(tuple => (tuple._1.index, tuple._2.index))
    }

    def excludeRegions(regions: List[(Int, Int)], exclusions: List[(Int, Int)]): List[(Int, Int)] = {
      def regionOverlapsWith(list: List[(Int, Int)])(region: (Int, Int)): Boolean = {
        val (start, end) = region
        
        list match {
          case Nil => false

          case x :: xs => (start >= x._1 && start <  x._2) ||
                          (end   >  x._1 && end   <= x._2) || regionOverlapsWith(xs)(region)
        }
      }

      regions match {
        case x :: xs if (regionOverlapsWith(exclusions)(x)) => excludeRegions(xs, exclusions)

        case x :: xs => x :: excludeRegions(xs, exclusions)

        case Nil => Nil
      }
    }
    
    def extract(regions: List[(Int, Int)]): List[(Int, Int, String)] = regions.map { region => (region._1, region._2, path.substring(region._1, region._2)) }
    
    val regexBoundaries = boundariesFor2("""(?<=^|\.|\])(/)""".r, """(?<=/[ixm]{0,3})($|\.|\[)""".r)
    
    val regexSegments = extract(regexBoundaries)
    val fieldSegments = extract(excludeRegions(boundariesFor1("""\.[^.\[/]+""".r), regexBoundaries))
    val arraySegments = extract(excludeRegions(boundariesFor1("""\[\d+\]""".r),    regexBoundaries))
    
    (regexSegments ++ fieldSegments ++ arraySegments).sortWith { (seg1, seg2) => seg1._1 < seg2._1 }.map(_._3)
  }
}

trait JPathImplicits {
  implicit def stringToJPath(s: String): JPath = JPath(s)
}
object JPathImplicits extends JPathImplicits