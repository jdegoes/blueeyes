package blueeyes.health.metrics

import blueeyes.json.JsonAST._

private[metrics] trait TimedAverageStatReport extends AsyncStatistic[Long, Map[Long, Double]]{
  def toJValue = details.map { details =>
    val buildDetails = details.toList.sortWith((e1, e2) => (e1._1 > e2._1))
    val safeInterval = if (intervalLengthInSeconds == 0) 1 else intervalLengthInSeconds
    val perSecond    = buildDetails.map(kv => JDouble(kv._2 / safeInterval))
    JObject(JField("perSecond", JObject(JField(config.toString, JArray(perSecond)) :: Nil)) :: Nil)
  }

  protected def config: IntervalConfig
  protected def intervalLengthInSeconds: Long
}

object TimedAverageStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: () => Long): AsyncStatistic[Long, Map[Long, Double]] = intervalConfig match{
    case e: interval => new TimedNumbersSample(e) with TimedAverageStatReport{
      val intervalLengthInSeconds        = config.granularity.unit.toSeconds(config.granularity.length)
    }

    case eternity    => new EternityTimedNumbersSample with TimedAverageStatReport{
      private val startTime = clock()

      def intervalLengthInSeconds = (clock() - startTime) / 1000
    }
  }
}