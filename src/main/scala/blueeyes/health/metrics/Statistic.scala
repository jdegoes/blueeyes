package blueeyes.health.metrics

import blueeyes.json.JsonAST.JValue

trait Statistic{
  def toJValue: JValue
}