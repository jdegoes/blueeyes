package blueeyes.health

import metrics.Counter
import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath
import blueeyes.util.Future

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
    montor.time("foo")({ Thread.sleep(10) })
    montor.timerStats.size must be (1)
    montor.timerStats.get(JPath("foo")).get.mean.ms.value must beCloseTo(10.0, precision * 3)
  }
  "records errors" in {
    montor.error("foo")(new NullPointerException())
    montor.errorStats.size must be (1)
    montor.errorStats.get(JPath("foo")).get.count mustEqual(1)
  }
  "moniors future time" in {
    montor.monitor("foo")(Future.async({ Thread.sleep(10) }))

    montor.timerStats.size must be (1)
    montor.timerStats.get(JPath("foo")).get.mean.ms.value must beCloseTo(10.0, precision * 3)
  }
  "moniors future error" in {
    val future = new Future[Unit]()
    montor.monitor("foo")(future)
    
    future.cancel(new NullPointerException())

    montor.errorStats.size must be (1)
    montor.errorStats.get(JPath("foo")).get.count mustEqual(1)
  }
  "does not monior future time when exception is thrown" in {
    montor.monitor("foo")(Future.async({ throw new NullPointerException() }))

    montor.timerStats.get(JPath("foo")).get.mean.ms.value mustEqual(0)
  }

  "traps error" in {
    try {
      montor.trap("foo") {throw new NullPointerException()}
    }
    catch {
      case t: Throwable =>
    }

    montor.errorStats.size must be (1)
    montor.errorStats.get(JPath("foo")).get.count mustEqual(1)
  }  
}