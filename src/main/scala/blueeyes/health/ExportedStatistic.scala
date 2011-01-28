package blueeyes.health

import metrics.Statistic
import blueeyes.json.JsonAST.JValue

class ExportedStatistic[T](value: => T)(implicit converter: T => JValue) extends Statistic[T, T]{
  def toJValue = converter(details)

  def details = value

  def count = 1l

  def +=(element: T) = this
}