package blueeyes

import json.serialization.Decomposer
import scalaz.Monoid

package object json {
  type JField = (String, JValue)

  def jarray(elements: JValue*): JValue = JArray(elements.toList)
  def jobject(fields: JField*): JValue = JObject(fields.toList)
  def jfield[A](name: String, value: A)(implicit d: Decomposer[A]): JField = JField(name, d(value))

  val MergeMonoid = new Monoid[JValue] {
    val zero = JUndefined

    def append(v1: JValue, v2: => JValue): JValue = v1.merge(v2)
  }
  
  val ConcatMonoid = new Monoid[JValue] {
    val zero = JUndefined

    def append(v1: JValue, v2: => JValue): JValue = v1 ++ v2
  }

  implicit val JObjectMergeMonoid = new Monoid[JObject] {
    val zero = JObject(Nil)

    def append(v1: JObject, v2: => JObject): JObject = v1.merge(v2).asInstanceOf[JObject]
  }
  
  private[json] def buildString(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
}

// vim: set ts=4 sw=4 et:
