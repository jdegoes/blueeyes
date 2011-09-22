package blueeyes.health

import metrics.eternity
import org.specs.Specification
import blueeyes.json.JPathImplicits._
import blueeyes.concurrent.Future
import blueeyes.json.JsonAST._
import blueeyes.json.{Printer, JPath}

class IntervalHealthMonitorSpec extends Specification with blueeyes.json.Implicits{

  private val montor = new IntervalHealthMonitor(eternity)

  "records count" in{
    montor.increment("foo")(2)
    montor.increment("foo")(3)

    montor.countStats.size must be (1)
    montor.countStats.get(JPath("foo")).get.count mustEqual(5)
  }

  "records the duration of the event" in {
    montor.time("foo")({ Thread.sleep(10) })
    montor.timerStats.size must be (1)
  }
  "records errors" in {
    montor.error("foo")(new NullPointerException())
    montor.errorStats.size must be (1)
  }
  "moniors future time" in {
    montor.monitor("foo")(Future.async({ Thread.sleep(10) }))

    montor.timerStats.size must eventually(be (1))
  }
  "moniors future error" in {
    val future = new Future[Unit]()
    montor.monitor("foo")(future)
    
    future.cancel(new NullPointerException())

    montor.errorStats.size must be (1)
  }
  "does not monior future time when exception is thrown" in {
    montor.monitor("foo")(Future.async({ throw new NullPointerException() }))
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
  }

  "composes into JValue" in{

    def export: Int = 2

    val config  = eternity
    val monitor = new IntervalHealthMonitor(config)
    monitor.increment("requestCount")(2)
    monitor.increment("requestCount")(3)
    monitor.export("request.export", export)

    println(Printer.pretty(Printer.render(monitor.toJValue)))
    val monitorJson = JObject(List(JField("requestCount", JObject(JField(config.toString, JArray(JInt(5) :: Nil)) :: Nil)), JField("request", JObject(List(JField("export", JInt(2)))))))

    monitor.toJValue mustEqual(monitorJson)
  }
}