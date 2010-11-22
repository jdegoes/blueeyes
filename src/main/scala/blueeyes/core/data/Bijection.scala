package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST.JValue

import scala.xml.NodeSeq
import scala.xml.XML

trait Bijection[T, S] { self =>
  def isDefinedAt(t: T): Boolean = {
    try {
      apply(t)
      
      true
    }
    catch {
      case _ => false
    }
  }
  
  def apply(t: T): S
  
  def unapply(s: S): T
  
  def inverse: Bijection[S, T] = new Bijection[S, T] {
    def apply(s: S): T   = self.unapply(s)
    def unapply(t: T): S = self.apply(t)
  }
  
  def compose[R](that: Bijection[R, T]): Bijection[R, S] = new Bijection[R, S] {
    def apply(r: R): S = self.apply(that.apply(r))
    
    def unapply(s: S): R = that.unapply(self.unapply(s))
  }
  
  def andThen[R](that: Bijection[S, R]): Bijection[T, R] = that.compose(self)
}

object Bijection {
  def identity[T]: Bijection[T, T] = new Bijection[T, T] {
    def apply(t: T): T = t
    
    def unapply(t: T): T = t
  }
}

trait BijectionsString {
  implicit val ByteArrayToString = new Bijection[Array[Byte], String] {
    def apply(t: Array[Byte]): String = t.map(_.toChar).mkString("")
    
    def unapply(s: String): Array[Byte] = s.toArray.map(_.toByte)
  }
  implicit val JValueToString = new Bijection[JValue, String] {
    def apply(s: JValue)   = compact(render(s))
    def unapply(t: String) = JsonParser.parse(t)
  }
  implicit val XMLToString = new Bijection[NodeSeq, String] {
    def apply(s: NodeSeq)  = s.toString
    def unapply(t: String) = XML.loadString(t)
  }
  
  implicit val StringToByteArray = ByteArrayToString.inverse
  implicit val StringToJValue    = JValueToString.inverse
  implicit val StringToXML       = XMLToString.inverse
  
  implicit val StringToString    = Bijection.identity[String]
}
object BijectionsString extends BijectionsString

trait BijectionsByteArray {
  implicit val StringToByteArray    = BijectionsString.StringToByteArray
  implicit val JValueToByteArray    = BijectionsString.JValueToString.andThen(BijectionsString.StringToByteArray)
  implicit val XMLToByteArray       = BijectionsString.XMLToString.andThen(BijectionsString.StringToByteArray)
  
  implicit val ByteArrayToString    = BijectionsString.ByteArrayToString
  implicit val ByteArrayToJValue    = JValueToByteArray.inverse
  implicit val ByteArrayToXML       = XMLToByteArray.inverse
  
  implicit val ByteArrayToByteArray = Bijection.identity[Array[Byte]]
}
object BijectionsByteArray extends BijectionsByteArray
