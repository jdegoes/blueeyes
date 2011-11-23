package blueeyes.health

import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import metrics.IntervalConfig
import blueeyes.json.MergeMonoid

import scalaz._
import Scalaz._
import blueeyes.concurrent.Future

class CompositeHealthMonitor(configs: List[IntervalConfig]) extends HealthMonitor with FunctionsMonitor{
  private implicit val mergeMonoid = MergeMonoid
  private val healthMonitors = configs.map(new IntervalHealthMonitor(_))

  def call(path: JPath) {healthMonitors.foreach(_.call(path))}

  def increment(path: JPath)(c: Long) {healthMonitors.foreach(_.increment(path)(c))}

  def count(path: JPath) {healthMonitors.foreach(_.count(path))}

  def trackTime(path: JPath)(ns: Long) {healthMonitors.foreach(_.trackTime(path)(ns))}

  def sample(path: JPath)(d: Double) {healthMonitors.foreach(_.sample(path)(d))}

  def export[T](path: JPath, value: => T)(implicit converter: (T) => JValue) {healthMonitors.foreach(_.export(path, value))}

  def error(path: JPath)(t: Throwable) {healthMonitors.foreach(_.error(path)(t))}

  def toJValue = Future[JValue](healthMonitors.map(_.toJValue): _*).map(_.asMA.sum)

  def shutdown(implicit timeout: akka.actor.Actor.Timeout) = akka.dispatch.Future.sequence(healthMonitors.map(_.shutdown), timeout.duration.toMillis).map(_ => ())
}
