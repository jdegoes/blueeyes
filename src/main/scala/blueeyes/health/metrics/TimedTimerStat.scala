package blueeyes.health.metrics

import blueeyes.json.JsonAST._
import blueeyes.concurrent.Future

import histogram.ValueStrategy._

private[metrics] trait TimedTimerStatReport extends AsyncStatistic[Long, Map[Long, Timer]]{
  def toJValue = details map {value =>
    val buildDetails  = value.toList.sortWith((e1, e2) => (e1._1 > e2._1)).map(_._2.toJValue).collect{case e: JObject => e}
    buildDetails match{
      case x :: xs =>
        val fieldsNames   = buildDetails.head.fields.map(_.name)
        val values        = fieldsNames.map{name => (name, buildDetails.map(_ \ name))}
        JObject(values.map(kv => JField(kv._1, JObject(JField(config.toString, JArray(kv._2)) :: Nil))))
      case Nil     => JObject(Nil)
    }
  }
  protected def config: IntervalConfig
}

object TimedTimerStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: () => Long): AsyncStatistic[Long, Map[Long, Timer]] = intervalConfig match{
    case e: interval => new TimedTimersSample(e) with TimedTimerStatReport
    case eternity    => new EternityTimedTimersSample with TimedTimerStatReport
  }
}

abstract class TimedTimersSample(intervalConfig: interval)(implicit clock: () => Long) extends TimedSample[Timer](intervalConfig)

abstract class EternityTimedTimersSample(implicit clock: () => Long) extends AsyncStatistic[Long, Map[Long, Timer]]{
  private val startTime = clock()
  private val _timer    = new Timer()

  def +=(element: Long) = {
    _timer.+=(element)
    this
  }

  def count = Future.sync(_timer.count)

  def details = Future.sync(Map[Long, Timer](startTime -> _timer))

  def shutdown() {}

  def config = eternity
}