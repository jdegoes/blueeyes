package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST.JValue

import scala.xml.NodeSeq
import scala.xml.XML

trait Bijection[T, S] extends Function1[T, S] { self =>
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

class ProxyBijection[T, S](val underlying: Bijection[T, S]) extends Bijection[T, S] {
  def apply(t: T): S = underlying.apply(t)
  
  def unapply(s: S): T = underlying.unapply(s)
}

object Bijection {
  def identity[T]: Bijection[T, T] = new Bijection[T, T] {
    def apply(t: T): T = t
    
    def unapply(t: T): T = t
  }
}

object Bijections {
  val ByteArrayToString = new Bijection[Array[Byte], String] {
    def apply(t: Array[Byte]): String = t.map(_.toChar).mkString("")
    
    def unapply(s: String): Array[Byte] = s.toArray.map(_.toByte)
  }
  val ByteArrayToByteArray = Bijection.identity[Array[Byte]]
  
  val StringToString    = Bijection.identity[String]
  val StringToByteArray = ByteArrayToString.inverse
    
  val JValueToString = new Bijection[JValue, String] {
    def apply(s: JValue)   = compact(render(s))
    def unapply(t: String) = JsonParser.parse(t)
  }
  val JValueToJValue    = Bijection.identity[JValue]
  val JValueToByteArray = JValueToString.andThen(StringToByteArray)
  
  val XMLToString = new Bijection[NodeSeq, String] {
    def apply(s: NodeSeq)  = s.toString
    def unapply(t: String) = XML.loadString(t)
  }  
  val XMLToXML        = Bijection.identity[NodeSeq]  
  val XMLToByteArray  = XMLToString.andThen(StringToByteArray)
}