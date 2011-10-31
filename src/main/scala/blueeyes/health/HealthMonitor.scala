package blueeyes.health

import blueeyes.concurrent.Future
import blueeyes.json.JPath
import blueeyes.json.JsonAST.{JValue, JNothing}

trait HealthMonitor { self => 
  def call(path: JPath)

  def increment(path: JPath)(c: Long)

  def count(path: JPath)

  def time[T](path: JPath)(f: => T): T

  def trackTime(path: JPath)(ns: Long)

  def timeFuture[T](path: JPath)(f: Future[T]): Future[T]

  def sample(path: JPath)(d: Double)

  def export[T](path: JPath, value: => T)(implicit converter: T => JValue)

  //def exportStats(path: JPath)(value: => Double)

  def error(path: JPath)(t: Throwable)

  def monitor[T](path: JPath)(f: Future[T]): Future[T]

  def trapFuture[T](path: JPath)(f: Future[T]): Future[T]

  def trap[T](path: JPath)(f: => T): T

  def withPrefix(prefix: JPath): HealthMonitor = new HealthMonitor {
    def call(path: JPath) = self.call(prefix \ path)
    def increment(path: JPath)(c: Long) = self.increment(prefix \ path)(c)
    def count(path: JPath) = self.count(prefix \ path)
    def time[T](path: JPath)(f: => T): T = self.time(prefix \ path)(f)
    def trackTime(path: JPath)(ns: Long) = self.trackTime(prefix \ path)(ns)
    def timeFuture[T](path: JPath)(f: Future[T]): Future[T] = self.timeFuture(prefix \ path)(f)
    def sample(path: JPath)(d: Double) = self.sample(prefix \ path)(d)
    def export[T](path: JPath, value: => T)(implicit converter: T => JValue) = self.export(prefix \ path, value)
    //def exportStats(path: JPath)(value: => Double) = self.exportStats(prefix \ path)(value)
    def error(path: JPath)(t: Throwable) = self.error(prefix \ path)(t)
    def monitor[T](path: JPath)(f: Future[T]): Future[T] = self.monitor(prefix \ path)(f)
    def trapFuture[T](path: JPath)(f: Future[T]): Future[T] = self.trapFuture(prefix \ path)(f)
    def trap[T](path: JPath)(f: => T): T = self.trap(prefix \ path)(f)

    def toJValue: Future[JValue] = self.toJValue
    def shutdown() = self.shutdown
  }

  def toJValue: Future[JValue]

  def shutdown()
}

object HealthMonitor {
  object Noop extends HealthMonitor {
    def call(path: JPath) = ()
    def increment(path: JPath)(c: Long) = ()
    def count(path: JPath) = ()
    def time[T](path: JPath)(f: => T): T = f
    def trackTime(path: JPath)(ns: Long) = ()
    def timeFuture[T](path: JPath)(f: Future[T]): Future[T] = f
    def sample(path: JPath)(d: Double) = ()
    def export[T](path: JPath, value: => T)(implicit converter: T => JValue) = ()
    //def exportStats(path: JPath)(value: => Double) = ()
    def error(path: JPath)(t: Throwable) = ()
    def monitor[T](path: JPath)(f: Future[T]): Future[T] = f
    def trapFuture[T](path: JPath)(f: Future[T]): Future[T] = f
    def trap[T](path: JPath)(f: => T): T = f

    override def withPrefix(prefix: JPath) = this
    def toJValue: Future[JValue] = Future.sync(JNothing)
    def shutdown() = ()
  }
}
