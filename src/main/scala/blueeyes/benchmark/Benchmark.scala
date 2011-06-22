package blueeyes.benchmark

import blueeyes.core.service.engines.HttpClientXLightWebEngines
import blueeyes.core.service.HttpClient
import blueeyes.demo.{BlueEyesDemo, Contact, BlueEyesDemoFacade}
import blueeyes.concurrent.Future
import net.lag.configgy.Configgy
import blueeyes.health.metrics.Timer
import java.util.concurrent.{TimeUnit, CountDownLatch}
import blueeyes.json.JsonParser.{parse => j}

object Benchmark extends ServerStart{ self =>

  class BlueEyesDemoFacadeImpl extends BlueEyesDemoFacade{
    def port = self.port
    val httpClient = new HttpClientXLightWebEngines{}
  }

  def main(args: Array[String]){
    startServer

    benchmark(1, report _, args(0).toInt)

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
    val blueEyesDemoFacade = new BlueEyesDemoFacadeImpl{}
    val future = blueEyesDemoFacade.health()

    val taskCounDown  = new CountDownLatch(1)

    future.deliverTo(response =>{
      println("********************************")
      println("*     Server Health report     *")
      println("********************************")
      println(blueeyes.json.Printer.pretty(blueeyes.json.JsonDSL.render(response.get)))

      taskCounDown.countDown
    })
    future.ifCanceled{v =>
      taskCounDown.countDown
    }
    taskCounDown.await
  }

  private def benchmark[T](connectionCount: Int, f: Timer => T, duration: Int): T= f(benchmark(new ContactStream().apply().take(connectionCount), duration))

  private def benchmark(stream: Stream[Contact], duration: Int): Timer = {
    val timer = new Timer()

    runBenchmarkTasks(stream, timer, duration)

    timer
  }

  private def runBenchmarkTasks(stream: Stream[Contact], timer: Timer, duration: Int){
    val benchmarkTasks = stream.map(startBenchmarkTask(_, timer, duration))

    benchmarkTasks.foreach(_.taskCounDown.await)
  }

  private def startBenchmarkTask(contact: Contact, timer: Timer, duration: Int) = {
    val benchmarkTask = new BenchmarkTask(new BlueEyesDemoFacadeImpl(), contact, timer, duration)
    new Thread(benchmarkTask).start
    benchmarkTask
  }
}

class BenchmarkTask(val clientFacade: BlueEyesDemoFacade, val contact: Contact, val timer: Timer, durationInSecs: Int) extends Runnable with HttpClientXLightWebEngines{
  val taskCounDown  = new CountDownLatch(1)
  def run = {
    val start = System.currentTimeMillis

    while (System.currentTimeMillis - start < durationInSecs * 1000){
      process({c: Contact => clientFacade.create(c)})
      process({c: Contact => clientFacade.remove(c.name)})
    }

    taskCounDown.countDown
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

trait ServerStart{
  var port = 8585
  private val configPattern = """server{
  port = %d
  sslPort = %d
}"""
  def startServer{
    var error: Option[Throwable] = None
    do{
      val doneSignal   = new CountDownLatch(1)

      Configgy.configureFromString(configPattern.format(port, port + 1))

      val startFuture = BlueEyesDemo.start
      startFuture.deliverTo { _ =>
        error = None
        doneSignal.countDown()
      }
      startFuture.ifCanceled{v =>
        error = v
        port  = port + 2
        doneSignal.countDown()
      }
    }while(error != None)

    error.foreach(throw _)
  }

  def stopServer = BlueEyesDemo.stop
}
