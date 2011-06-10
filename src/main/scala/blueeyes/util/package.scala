package blueeyes

import org.joda.time.DateTime
import scala.math.Ordering

package object util {
  implicit object DateTimeOrdering extends Ordering[DateTime] {
    override def compare(d1: DateTime, d2: DateTime) = d1.compareTo(d2)
  }
}



// vim: set ts=4 sw=4 et:
