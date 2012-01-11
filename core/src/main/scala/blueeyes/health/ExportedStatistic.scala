package blueeyes.health

import blueeyes.json.JsonAST.JValue
import metrics.{SyncStatistic}

class ExportedStatistic[T](value: => T)(implicit converter: T => JValue) extends SyncStatistic[T, T]{
  def toJValue = converter(details)

  def details = value

  def count = 1l

  def +=(element: T) = this
}
