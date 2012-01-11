package blueeyes.benchmark

import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Duration

import blueeyes.bkka.AkkaDefaults
import blueeyes.core.service.engines.HttpClientXLightWeb
import blueeyes.demo.{Contact, BlueEyesDemoFacade}
import blueeyes.health.metrics.Timer
import blueeyes.json.JsonAST.JValue

import java.util.concurrent.{CountDownLatch, ThreadPoolExecutor, SynchronousQueue, TimeUnit}

object Benchmark extends BenchmarkServer with AkkaDefaults { self =>
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

    Await.result(
      future onSuccess { case Some(jvalue) =>
        println(title)
        println(blueeyes.json.Printer.pretty(blueeyes.json.JsonDSL.render(jvalue)))
      },
      Duration.Inf
    ) 
  }

  private def benchmark[T](connectionCount: Int, f: Timer => T, duration: Int): T= f(benchmark(new ContactStream().apply().take(connectionCount).toList, duration))

  private def benchmark(contacts: List[Contact], duration: Int): Timer = {
    val timer = new Timer()

    runBenchmarkTasks(contacts, timer, duration)

    timer
  }

  private def runBenchmarkTasks(contacts: List[Contact], timer: Timer, duration: Int){
    contacts.map(startBenchmarkTask(_, timer, duration))
  }

  private def startBenchmarkTask(contact: Contact, timer: Timer, duration: Int) = {
    val benchmarkTask = new BenchmarkTask(new BlueEyesDemoFacadeImpl(), contact, timer, duration)
    executorService.submit(benchmarkTask)
    benchmarkTask
  }
}

class BenchmarkTask(val clientFacade: BlueEyesDemoFacade, val contact: Contact, val timer: Timer, durationInSecs: Int) extends HttpClientXLightWeb with Runnable with AkkaDefaults {
  def run = {
    val start = System.currentTimeMillis

    while (System.currentTimeMillis - start < durationInSecs * 1000){
      process({c: Contact => clientFacade.create(c)})
      process({c: Contact => clientFacade.remove(c.name)})
    }
  }

  private def process[T](f: Contact => Future[T]) = {
    val countDown = new CountDownLatch(1)
    timer.time {
      Await.result(f(contact) onFailure { case ex => ex.printStackTrace }, Duration.Inf)
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
