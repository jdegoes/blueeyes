package blueeyes.health.metrics

import blueeyes.json.JsonAST._
import blueeyes.util.Clock
import akka.actor.Actor
import akka.dispatch.Future
import akka.util.Timeout

private[metrics] trait TimedAverageStatReport extends AsyncStatistic[Long, Map[Long, Double]] {
  def toJValue = details.map { details =>
    val buildDetails = details.toList.sortWith((e1, e2) => (e1._1 > e2._1))
    val safeInterval = if (intervalLengthInSeconds == 0) 1 else intervalLengthInSeconds
    val perSecond    = buildDetails.map(kv => JNum(kv._2 / safeInterval))
    JObject(JField("perSecond", JObject(JField(config.toString, JArray(perSecond)) :: Nil)) :: Nil)
  }

  def config: IntervalConfig
  def intervalLengthInSeconds: Long
}

object TimedAverageStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: Clock): AsyncStatistic[Long, Map[Long, Double]] = intervalConfig match {
    case e: interval => new TimedNumbersSample(e) with TimedAverageStatReport {
      val intervalLengthInSeconds = config.granularity.unit.toSeconds(config.granularity.length)
    }

    case eternity => 
      val sample = new EternityTimedNumbersSample
      new WrapAsyncStatistic(sample) with TimedAverageStatReport {
        val startTime = clock.now().getMillis
        def intervalLengthInSeconds = (clock.now().getMillis - startTime) / 1000
        def config: IntervalConfig = sample.config
      }
  }
}

private[metrics] trait TimedCountStatReport extends AsyncStatistic[Long, Map[Long, Double]]{
  def toJValue = details.map {details => JObject(JField(config.toString, JArray(details.toList.sortWith((e1, e2) => (e1._1 > e2._1)).map(kv => JNum(kv._2.toLong)))) :: Nil) }

  protected def config: IntervalConfig
}

object TimedCountStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: Clock): AsyncStatistic[Long, Map[Long, Double]] = intervalConfig match {
    case e: interval => new TimedNumbersSample(e) with TimedCountStatReport 

    case eternity    => 
      val sample = new EternityTimedNumbersSample 
      new WrapAsyncStatistic(sample) with TimedCountStatReport {
        val startTime = clock.now().getMillis
        def intervalLengthInSeconds = (clock.now().getMillis - startTime) / 1000
        def config: IntervalConfig = sample.config
      }
  }
}

private[metrics] trait TimedErrorStatReport extends AsyncStatistic[Long, Map[Long, Double]]{
  def toJValue = details map {details =>JObject(JField(config.toString, JArray(details.toList.sortWith((e1, e2) => (e1._1 > e2._1)).map(kv => JNum(kv._2.toLong)))) :: Nil) }

  protected def config: IntervalConfig
}

object TimedErrorStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: Clock): AsyncStatistic[Long, Map[Long, Double]] = intervalConfig match{
    case e: interval => new TimedNumbersSample(e) with TimedErrorStatReport 

    case eternity    => 
      val sample = new EternityTimedNumbersSample 
      new WrapAsyncStatistic(sample) with TimedErrorStatReport {
        val startTime = clock.now().getMillis
        def intervalLengthInSeconds = (clock.now().getMillis - startTime) / 1000
        def config: IntervalConfig = sample.config
      }
  }
}

import histogram.ValueStrategy._

private[metrics] trait TimedTimerStatReport extends AsyncStatistic[Long, Map[Long, Timer]]{
  def toJValue = details map {value =>
    val buildDetails  = value.toList.sortWith((e1, e2) => (e1._1 > e2._1)).map(_._2.toJValue).collect{case e: JObject => e}
    buildDetails match{
      case x :: xs =>
        val fieldsNames   = buildDetails.head.fields.map(_._1)
        val values        = fieldsNames.map{name => (name, buildDetails.map(_ \ name))}
        JObject(values.map(kv => JField(kv._1, JObject(JField(config.toString, JArray(kv._2)) :: Nil))))
      case Nil     => JObject(Nil)
    }
  }
  protected def config: IntervalConfig
}

object TimedTimerStat {
  def apply(intervalConfig: IntervalConfig)(implicit clock: Clock): AsyncStatistic[Long, Map[Long, Timer]] = intervalConfig match {
    case e: interval => new TimedTimersSample(e) with TimedTimerStatReport 

    case eternity    => new EternityTimedTimersSample with TimedTimerStatReport
  }
}

abstract class TimedTimersSample(intervalConfig: interval)(implicit clock: Clock) extends TimedSample[Timer](intervalConfig)

abstract class EternityTimedTimersSample(implicit clock: Clock) extends AsyncStatistic[Long, Map[Long, Timer]] {
  private val startTime = clock.now.getMillis()
  private val _timer    = new Timer()

  def +=(element: Long) = {
    _timer.+=(element)
    this
  }

  def count = Future(_timer.count)

  def details = Future(Map[Long, Timer](startTime -> _timer))

  def shutdown(timeout: Timeout) = Future(())

  def config = eternity
}
