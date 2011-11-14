package blueeyes.health

import metrics.{IntervalLength, eternity, interval}
import org.specs2.mutable.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.concurrent.Future
import blueeyes.json.JsonAST._
import blueeyes.json.{JsonParser, Printer, JPath}
import java.util.concurrent.TimeUnit

class IntervalHealthMonitorSpec extends Specification with blueeyes.json.Implicits{

  private val monitor = new IntervalHealthMonitor(eternity)

  "records count" in{
    monitor.increment("foo")(2)
    monitor.increment("foo")(3)

    monitor.countStats.size must_== (1)
    monitor.countStats.get(JPath("foo")).get.count.value must eventually (beSome(5))
  }

  "records the duration of the event" in {
    monitor.time("foo")({ Thread.sleep(10) })
    monitor.timerStats.size must_== (1)
  }
  "records errors" in {
    monitor.error("foo")(new NullPointerException())
    monitor.errorStats.size must_== (1)
  }
  "moniors future time" in {
    monitor.monitor("foo")(Future.async({ Thread.sleep(10) }))

    monitor.timerStats.size must eventually(be_==(1))
  }
  "moniors future error" in {
    val future = new Future[Unit]()
    monitor.monitor("foo")(future)
    
    future.cancel(new NullPointerException())

    monitor.errorStats.size must_== (1)
  }

  "records sample event" in {
    monitor.sample("foo")(1.1)
    monitor.sampleStats.size must_== (1)
    monitor.sampleStats.get(JPath("foo")).get.count mustEqual(1)
  }

  "traps error" in {
    try {
      monitor.trap("foo") {throw new NullPointerException()}
    }
    catch {
      case t: Throwable =>
    }

    monitor.errorStats.size must_== (1)
  }

  "composes errors into JValue as array" in{
    val config  = interval(IntervalLength(3, TimeUnit.SECONDS), 3)
    val monitor = new IntervalHealthMonitor(config)
    monitor.error("foo")(new NullPointerException())

    val monitorJson = JsonParser.parse("""{"foo":{"errorDistribution":{"java.lang.NullPointerException":{"3s x 3":[1,0,0]}},"count":{"3s x 3":[1,0,0]}}}""")
    val jValue = monitor.toJValue
    jValue.value must eventually(beSome(monitorJson))
  }
  "composes into JValue" in{

    def export: Int = 2

    val config  = eternity
    val monitor = new IntervalHealthMonitor(config)
    monitor.increment("requestCount")(2)
    monitor.increment("requestCount")(3)
    monitor.export("request.export", export)

    val monitorJson = JObject(List(JField("requestCount", JObject(JField(config.toString, JArray(JInt(5) :: Nil)) :: Nil)), JField("request", JObject(List(JField("export", JInt(2)))))))

    monitor.toJValue.value must eventually(beSome(monitorJson))
  }
}