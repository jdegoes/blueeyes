package blueeyes

import scalaz.Monoid

package object json{
  import JsonAST.{JValue, JNothing, JObject}

  val MergeMonoid = new Monoid[JValue] {
    val zero = JNothing

    def append(v1: JValue, v2: => JValue): JValue = v1.merge(v2)
  }
  val ConcatMonoid = new Monoid[JValue] {
    val zero = JNothing

    def append(v1: JValue, v2: => JValue): JValue = v1 ++ v2
  }
  implicit val JObjectMergeMonoid = new Monoid[JObject] {
    val zero = JObject(Nil)

    def append(v1: JObject, v2: => JObject): JObject = v1.merge(v2).asInstanceOf[JObject]
  }
}