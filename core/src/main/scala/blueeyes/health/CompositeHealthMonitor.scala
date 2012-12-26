package blueeyes.health

import blueeyes.json.JPath
import blueeyes.json._
import blueeyes.json.MergeMonoid
import metrics.IntervalConfig

import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import akka.util.Timeout

import scalaz._
import scalaz.std.list._
import scalaz.syntax.foldable._

class CompositeHealthMonitor(configs: List[IntervalConfig])(implicit executor: ExecutionContext) extends HealthMonitor with FunctionsMonitor {
  private implicit val mergeMonoid = MergeMonoid
  private val healthMonitors = configs.map(new IntervalHealthMonitor(_))

  def call(path: JPath) {healthMonitors.foreach(_.call(path))}

  def increment(path: JPath)(c: Long) {healthMonitors.foreach(_.increment(path)(c))}

  def count(path: JPath) {healthMonitors.foreach(_.count(path))}

  def trackTime(path: JPath)(ns: Long) {healthMonitors.foreach(_.trackTime(path)(ns))}

  def sample(path: JPath)(d: Double) {healthMonitors.foreach(_.sample(path)(d))}

  def export[T](path: JPath, value: => T)(implicit converter: (T) => JValue) {healthMonitors.foreach(_.export(path, value))}

  def error(path: JPath)(t: Throwable) {healthMonitors.foreach(_.error(path)(t))}

  def toJValue = Future.sequence[JValue, List](healthMonitors.map(_.toJValue))map(_.suml)

  def shutdown(timeout: Timeout): Future[Any] = Future.sequence[Any, List](healthMonitors.map(_.shutdown(timeout)))
}
