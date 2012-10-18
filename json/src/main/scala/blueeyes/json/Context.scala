package blueeyes.json

// context used by JParser, taken from jawn under MIT license.
// (https://github.com/non/jawn)

import scala.collection.mutable

private[json] sealed trait Context {
  def add(s: String): Unit
  def add(v: JValue): Unit
  def finish: JValue
  def isObj: Boolean
}

private[json] final class SingleContext extends Context {
  var value: JValue = null
  def add(s: String): Unit = value = JString(s)
  def add(v: JValue): Unit = value = v
  def finish = value
  def isObj = false
}

private[json] final class ArrContext extends Context {
  private val vs = mutable.ListBuffer.empty[JValue]

  def add(s: String): Unit = vs.append(JString(s))
  def add(v: JValue): Unit = vs.append(v)
  def finish = new JArray(vs.toList)
  def isObj = false
}

private[json] final class ObjContext extends Context {
  private var key: String = null
  private val vs = mutable.Map.empty[String, JValue]

  def add(s: String): Unit = if (key == null) {
    key = s
  } else {
    vs(key) = JString(s)
    key = null
  }

  def add(v: JValue): Unit = { vs(key) = v; key = null }

  def finish = JObject(vs)
  def isObj = true
}
