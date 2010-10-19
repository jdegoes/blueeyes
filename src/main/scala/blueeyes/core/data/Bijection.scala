package blueeyes.core.data

trait Bijection[T, S] extends Function1[T, S] { self =>
  def apply(t: T): S
  
  def unapply(s: S): T
  
  def inverse: Bijection[S, T] = new Bijection[S, T] {
    def apply(s: S): T   = self.unapply(s)
    def unapply(t: T): S = self.apply(t)
  }
}

trait DataTranscoder[T, S] {
  def transcode: Bijection[T, S]
}

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST.JValue
import xml.NodeSeq
import xml.XML
object Bijections{
  def textToJson = new Bijection[String, JValue]{
    def unapply(s: JValue) = compact(render(s))
    def apply(t: String)   = JsonParser.parse(t)
  }

  def textToXML = new Bijection[String, NodeSeq]{
    def unapply(s: NodeSeq) = s.toString
    def apply(t: String)    = XML.loadString(t)
  }

  def textToText = new Bijection[String, String]{
    def unapply(s: String) = s
    def apply(t: String)   = t
  }
}