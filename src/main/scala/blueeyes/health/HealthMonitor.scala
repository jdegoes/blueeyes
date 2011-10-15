package blueeyes.health

import blueeyes.concurrent.Future
import blueeyes.json.JPath
import blueeyes.json.JsonAST.JValue

trait HealthMonitor{
  def request(path: JPath)

  def increment(path: JPath)(c: Long)

  def count(path: JPath)

  def time[T](path: JPath)(f: => T): T

  def trackTime(path: JPath)(ns: Long)

  def timeFuture[T](path: JPath)(f: Future[T]): Future[T]

  def sample(path: JPath)(d: Double)

  def export[T](path: JPath, value: => T)(implicit converter: T => JValue)

  def error[T <: Throwable](path: JPath)(t: T)

  def monitor[T](path: JPath)(f: Future[T]): Future[T]

  def trapFuture[T](path: JPath)(f: Future[T]): Future[T]

  def trap[T](path: JPath)(f: => T): T

  def toJValue: Future[JValue]

  def shutdown()
}