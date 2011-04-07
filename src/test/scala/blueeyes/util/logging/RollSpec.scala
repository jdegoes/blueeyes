package blueeyes.util.logging

import RollPolicies._
import org.specs.Specification
import java.util.{Calendar, GregorianCalendar}

class RollSpec extends Specification with Roll{
  "Roll: calculates nextRollTime with Never policy" in {
    val next = calendar(1)
    next.add(Calendar.YEAR, 100)

    nextRollTime(Never, 1) mustEqual(next.getTimeInMillis)
  }
  "Roll: calculates nextRollTime with Hourly policy" in {
    val next = calendar(1)
    next.add(Calendar.HOUR_OF_DAY, 1)

    nextRollTime(Hourly, 1) mustEqual(next.getTimeInMillis)
  }
  "Roll: calculates nextRollTime with Daily policy" in {
    val next = calendar(1)
    next.set(Calendar.HOUR_OF_DAY, 0)
    next.add(Calendar.DAY_OF_MONTH, 1)

    nextRollTime(Daily, 1) mustEqual(next.getTimeInMillis)
  }
  "Roll: calculates nextRollTime with Weekly policy" in {
    val next = calendar(1)
    next.set(Calendar.HOUR_OF_DAY, 0)
    do {
      next.add(Calendar.DAY_OF_MONTH, 1)
    } while (next.get(Calendar.DAY_OF_WEEK) != 1)

    nextRollTime(Weekly(1), 1) mustEqual(next.getTimeInMillis)
  }

  private def calendar(now: Long) = {
    val next = new GregorianCalendar()

    next.setTimeInMillis(now)
    next.set(Calendar.MILLISECOND, 0)
    next.set(Calendar.SECOND, 0)
    next.set(Calendar.MINUTE, 0)

    next
  }

}