package blueeyes.health.metrics

import blueeyes.json.JsonAST._

private[metrics] trait TimedAverageStatReport extends Statistic[Long, Map[Long, Double]]{
  def toJValue = {
    val buildDetails = details.toList.sortWith((e1, e2) => (e1._1 > e2._1))
    val safeInterval = if (intervalLengthInSeconds == 0) 1 else intervalLengthInSeconds
    val perSecond    = buildDetails.map(kv => JDouble(kv._2 / safeInterval))
    JObject(JField("perSecond", JObject(JField(config.toString, JArray(perSecond)) :: Nil)) :: Nil)
  }
  protected def config: IntervalConfig
  protected def intervalLengthInSeconds: Long
}

object TimedAverageStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: () => Long): Statistic[Long, Map[Long, Double]] = intervalConfig match{
    case e: interval => new TimedNumbersSample(e) with TimedAverageStatReport

    case eternity    => new EternityTimedNumbersSample with TimedAverageStatReport
  }
}