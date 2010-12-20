package blueeyes.health

import metrics.Counter
import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath
import blueeyes.util.Future
import blueeyes.json.JsonAST._

class HealthMonitorSpec extends Specification{

  private val precision = 5.0

  private val montor = new HealthMonitor()

  "records count" in{
    montor.increment("foo")(2)
    montor.increment("foo")(3)

    montor.countStats.size must be (1)
    montor.countStats.get(JPath("foo")).get.count mustEqual(5)
  }

  "records the duration of the event" in {
    montor.time("foo")({ Thread.sleep(10) })
    montor.timerStats.size must be (1)
    montor.timerStats.get(JPath("foo")).get.mean.ms.value must notBe(0.0)
  }
  "records errors" in {
    montor.error("foo")(new NullPointerException())
    montor.errorStats.size must be (1)
    montor.errorStats.get(JPath("foo")).get.count mustEqual(1)
  }
  "moniors future time" in {
    montor.monitor("foo")(Future.async({ Thread.sleep(10) }))

    montor.timerStats.size must be (1)
    montor.timerStats.get(JPath("foo")).get.mean.ms.value must notBe(0.0)
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

  "records sample event" in {
    montor.sample("foo")(1.1)
    montor.sampleStats.size must be (1)
    montor.sampleStats.get(JPath("foo")).get.count mustEqual(1)
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

  "composes into JValue" in{

    val monitor = new HealthMonitor()
    monitor.increment("requestCount")(2)
    monitor.increment("requestCount")(3)

    val monitorJson = JObject(List(JField("requestCount",JInt(5))))

    monitor.toJValue mustEqual(monitorJson)
  }
}