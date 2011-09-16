package blueeyes.health.metrics

import blueeyes.json.JsonAST._
import java.util.concurrent.atomic.AtomicLong

private[metrics] trait TimedTimerStatReport extends Statistic[Long, Map[Long, Timer]]{
  def toJValue = {
    val buildDetails  = details.toList.sortWith((e1, e2) => (e1._1 > e2._1)).map(_._2.toJValue).collect{case e: JObject => e}
    val fieldsNames   = buildDetails.head.fields.map(_.name)
    val values        = fieldsNames.map{name => (name, buildDetails.map(_ \ name))}
    JObject(values.map(kv => JField(kv._1, JObject(JField(config.toString, JArray(kv._2)) :: Nil))))
  }
  protected def config: IntervalConfig
}

object TimedTimerStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: () => Long): Statistic[Long, Map[Long, Timer]] = intervalConfig match{
    case e: interval => new TimedTimersSample(e) with TimedTimerStatReport
    case eternity    => new EternityTimedTimersSample with TimedTimerStatReport
  }
}

trait TimerHistogram extends Histogram[Timer]{
  def bucketInitialValue = new Timer()

  def setBucketValue(value: Timer, ns: Long) = value.+=(ns)
}

abstract class TimedTimersSample(intervalConfig: interval)(implicit clock: () => Long) extends TimedSample[Timer](intervalConfig) with TimerHistogram{
  protected def +=(value: AtomicLong, elem: Long) {value.compareAndSet(0l, elem)}
}

abstract class EternityTimedTimersSample(implicit clock: () => Long) extends Statistic[Long, Map[Long, Timer]]{
  private val startTime = clock()
  private val _timer    = new Timer()

  def +=(element: Long) = {
    _timer.+=(element)
    this
  }

  def count = _timer.count

  def details = Map[Long, Timer](startTime -> _timer)

  def config = eternity
}