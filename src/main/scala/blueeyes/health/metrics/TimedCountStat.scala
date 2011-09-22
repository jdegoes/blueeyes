package blueeyes.health.metrics

import blueeyes.json.JsonAST._

private[metrics] trait TimedCountStatReport extends Statistic[Long, Map[Long, Double]]{
  def toJValue = {
    val buildDetails  = details.toList.sortWith((e1, e2) => (e1._1 > e2._1))
    JObject(JField(config.toString, JArray(buildDetails.map(kv => JInt(kv._2.toLong)))) :: Nil)
  }

  protected def config: IntervalConfig
}

object TimedCountStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: () => Long): Statistic[Long, Map[Long, Double]] = intervalConfig match{
    case e: interval => new TimedNumbersSample(e) with TimedCountStatReport
    case eternity    => new EternityTimedNumbersSample with TimedCountStatReport
  }
}