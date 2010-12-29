package blueeyes.benchmark

import blueeyes.core.service.engines.HttpClientXLightWebEnginesArrayByte
import blueeyes.core.service.HttpClient
import blueeyes.demo.{BlueEyesDemo, Contact, ContactListFacade}
import blueeyes.util.Future
import net.lag.configgy.Configgy
import blueeyes.health.metrics.Timer
import java.util.concurrent.{TimeUnit, CountDownLatch}
import blueeyes.json.JsonParser.{parse => j}

object Benchmark extends ServerStart{

  private val threadsCount = 4
  private val contactsSize = 200

  def main(args: Array[String]){
    startServer

    println("Execution Time: " + benchmark(ContactStream(threadsCount, contactsSize)))

    stopServer

    System.exit(0)
  }

  private def benchmark(streams: List[Stream[Contact]]) = {
    val timer = new Timer()

    runBenchmarkTasks(streams, timer)

    timer.mean.convert(TimeUnit.MILLISECONDS).value    
  }

  private def runBenchmarkTasks(streams: List[Stream[Contact]], timer: Timer){
    val benchmarkTasks = streams.map(startBenchmarkTask(_, timer))

    benchmarkTasks.foreach(_.taskCounDown.await)
  }

  private def startBenchmarkTask(contactsStream: Stream[Contact], timer: Timer) = {
    val benchmarkTask = new BenchmarkTask(port, contactsStream, timer)
    new Thread(benchmarkTask).start
    benchmarkTask
  }
}

class BenchmarkTask(val port: Int, val contactsStream: Stream[Contact], val timer: Timer) extends Runnable with ContactListFacade with HttpClientXLightWebEnginesArrayByte{
  val taskCounDown  = new CountDownLatch(1)
  def run = {
    timer.time{
      process({c: Contact => create(c)})
      process({c: Contact => list})
      process({c: Contact => contact(c.name)})
      process({c: Contact => search(j("""{ "name" : "%s" }""".format(c.name)))})
      process({c: Contact => remove(c.name)})
    }

    taskCounDown.countDown
  }

  private def process[T](f: Contact => HttpClient[Array[Byte]] => Future[T]) = {
    contactsStream.foreach(contact => {
      val countDown      = new CountDownLatch(1)
      val future    = exec[T](f(contact))
      future.deliverTo(response =>{
        countDown.countDown
      })
      future.ifCanceled{v =>
        countDown.countDown
        v.foreach(_.printStackTrace)
      }
      countDown.await
    })
  }
}

object ContactStream{
  def apply(count: Int, size: Int): List[Stream[Contact]] = {
    val contacts = for (i <- 1 to count) yield new ContactStream(i).apply().take(size)
    contacts.toList
  }
}

class ContactStream(streamIndex: Int){
  private var index = 0

  def apply(): Stream[Contact] = {
    index = index + 1

    Stream.cons(contact, apply())
  }

  private def contact = Contact(>>(baseName), Some(<<(baseEmail)), Some(>>(baseCountry)), Some(>>(baseCity)), Some(>>(baseAddress)))

  private def >>(value: String) = value + streamIndex + "_" + index

  private def <<(value: String) = index + streamIndex + "_" + value

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
