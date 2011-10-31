package blueeyes.health

import blueeyes.json.JPath
import blueeyes.util.ClockSystem
import blueeyes.concurrent.Future

private[health] trait FunctionsMonitor {
  def time[T](path: JPath)(f: => T): T      = {
    val startTime = ClockSystem.realtimeClock.nanoTime()
    val t = f
    trackTime(path)(ClockSystem.realtimeClock.nanoTime - startTime)
    t
  }

  def timeFuture[T](path: JPath)(f: Future[T]): Future[T] = {
    val startTime = ClockSystem.realtimeClock.nanoTime()
    f.deliverTo(v => {
      trackTime(path)(ClockSystem.realtimeClock.nanoTime - startTime)
    })
  }

  def trapFuture[T](path: JPath)(f: Future[T]): Future[T] = {
    f.ifCanceled(_.foreach(error(path)(_)))
    f
  }

  def trap[T](path: JPath)(f: => T): T = {
    try {
      f
    }
    catch {
      case t: Throwable => error(path)(t)
      throw t
    }
  }

  def monitor[T](path: JPath)(f: Future[T]): Future[T] = {
    trapFuture(path)(f)
    timeFuture(path)(f)
  }

  def error(path: JPath)(t: Throwable)

  def trackTime(path: JPath)(ns: Long)
}
