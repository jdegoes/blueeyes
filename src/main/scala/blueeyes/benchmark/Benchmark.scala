package blueeyes.benchmark

import blueeyes.core.service.engines.HttpClientXLightWeb
import blueeyes.concurrent.Future
import net.lag.configgy.Configgy
import blueeyes.health.metrics.Timer
import java.util.concurrent.{CountDownLatch, ThreadPoolExecutor, SynchronousQueue, TimeUnit}
import blueeyes.json.JsonAST.JValue
import blueeyes.demo.{Contact, BlueEyesDemoFacade}

object Benchmark extends ServerStart{ self =>

  val executorService = new ThreadPoolExecutor(20, 100, 10*60, TimeUnit.SECONDS, new SynchronousQueue())

  def main(args: Array[String]){
    startServer(if (args.size > 2) args(2).toBoolean else false)

    benchmark(args(0).toInt, report _, args(1).toInt)

    stopServer

    System.exit(0)
  }

  private def report(timer: Timer){
    healthReport
    clientReport(timer)
  }
  private def clientReport(timer: Timer){
    println("********************************")
    println("*     Client Health report     *")
    println("********************************")
    println("test time:            " + timer.total.convert(TimeUnit.SECONDS).time + " seconds")
    println("requests count:       " + timer.count)
    println("request average time: " + timer.mean.convert(TimeUnit.MILLISECONDS).time + " milliseconds")
    println("requests per second:  " + (timer.count / timer.total.convert(TimeUnit.SECONDS).time))
  }

  private def healthReport{
    val blueEyesDemoFacade = new BlueEyesDemoFacadeImpl()

    val serviceHealthTitle =
"""********************************
*     Service Health report    *
********************************"""
    val serverHealthTitle =
"""********************************
*     Server Health report     *
********************************"""

    responseReport(blueEyesDemoFacade.health, serviceHealthTitle)
    responseReport(blueEyesDemoFacade.serverHealth, serverHealthTitle)
  }

  private def responseReport(future: Future[Option[JValue]], title: String) = {
    val taskCounDown  = new CountDownLatch(1)

    future.deliverTo(response =>{
      println(title)
      println(blueeyes.json.Printer.pretty(blueeyes.json.JsonDSL.render(response.get)))
      taskCounDown.countDown
    })
    future.ifCanceled{v =>
      taskCounDown.countDown
    }
    taskCounDown.await
  }

  private def benchmark[T](connectionCount: Int, f: Timer => T, duration: Int): T= f(benchmark(new ContactStream().apply().take(connectionCount).toList, duration))

  private def benchmark(contacts: List[Contact], duration: Int): Timer = {
    val timer = new Timer()

    runBenchmarkTasks(contacts, timer, duration)

    timer
  }

  private def runBenchmarkTasks(contacts: List[Contact], timer: Timer, duration: Int){
    val benchmarkFutures = Future[Unit](contacts.map(startBenchmarkTask(_, timer, duration).future): _*)

    val countDownLatch = new CountDownLatch(1)
    benchmarkFutures.deliverTo(v => countDownLatch.countDown)
    countDownLatch.await
  }

  private def startBenchmarkTask(contact: Contact, timer: Timer, duration: Int) = {
    val benchmarkTask = new BenchmarkTask(new BlueEyesDemoFacadeImpl(), contact, timer, duration)
    executorService.submit(benchmarkTask)
    benchmarkTask
  }
}

class BenchmarkTask(val clientFacade: BlueEyesDemoFacade, val contact: Contact, val timer: Timer, durationInSecs: Int) extends HttpClientXLightWeb with Runnable {
  val future  = new Future[Unit]()
  def run = {
    val start = System.currentTimeMillis

    while (System.currentTimeMillis - start < durationInSecs * 1000){
      process({c: Contact => clientFacade.create(c)})
      process({c: Contact => clientFacade.remove(c.name)})
    }

    future.deliver(())
  }

  private def process[T](f: Contact => Future[T]) = {
    val countDown = new CountDownLatch(1)
    timer.time{
      val future    = f(contact)
      future.deliverTo(response =>{
        countDown.countDown
      })
      future.ifCanceled{v =>
        countDown.countDown
        v.foreach(_.printStackTrace)
      }
      countDown.await
    }
  }
}

class ContactStream(){
  private var index = 0

  def apply(): Stream[Contact] = {
    index = index + 1

    Stream.cons(contact, apply())
  }

  private def contact = Contact(>>(baseName), Some(<<(baseEmail)), Some(>>(baseCountry)), Some(>>(baseCity)), Some(>>(baseAddress)))

  private def >>(value: String) = value + "_" + index

  private def <<(value: String) = index + "_" + value

  def baseName    = "John_"

  def baseEmail   = "_john@google.com"

  def baseCountry = "UK_"

  def baseCity    = "London_"

  def baseAddress = "Streat "
}
