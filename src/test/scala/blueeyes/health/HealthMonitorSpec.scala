package blueeyes.health

import metrics.Counter
import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath

class HealthMonitorSpec extends Specification{

  private val precision = 5.0

  private val montor = new HealthMonitor{}

  "records count" in{
    montor.count("foo")(2)
    montor.count("foo")(3)

    montor.countStats.size must be (1)
    montor.countStats.get(JPath("foo")).get.count mustEqual(5)
  }

  "records the duration of the event" in {
    montor.time(JPath("foo"))({ Thread.sleep(10) })
    montor.timerStats.size must be (1)
    montor.timerStats.get(JPath("foo")).get.mean.ms.value must beCloseTo(10.0, precision * 3)
  }
}