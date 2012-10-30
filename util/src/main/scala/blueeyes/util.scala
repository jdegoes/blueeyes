package blueeyes

import org.joda.time.{DateTime, Instant, ReadableDuration}
import scala.math.Ordering
import scalaz._

package object util {
  implicit object DateTimeOrdering extends Ordering[DateTime] {
    override def compare(d1: DateTime, d2: DateTime) = d1.compareTo(d2)
  }

  implicit object InstantOrdering extends Ordering[Instant] {
    override def compare(d1: Instant, d2: Instant) = d1.compareTo(d2)
  }

  implicit object ReadableDurationOrdering extends Ordering[ReadableDuration] {
    override def compare(d1: ReadableDuration, d2: ReadableDuration) = d1.compareTo(d2)
  }

  implicit val InstantOrder = scalaz.std.anyVal.longInstance.contramap((i: Instant) => i.getMillis)

  sealed trait Kestrel[A] {
    protected def a: A
    def tap(f: A => Unit): A = { f(a); a }
  }

  implicit def any2Kestrel[A](a0: => A): Kestrel[A] = new Kestrel[A] {
    lazy val a = a0
  }
}




// vim: set ts=4 sw=4 et:
