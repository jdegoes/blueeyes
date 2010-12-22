package blueeyes.benchmark

import blueeyes.core.service.engines.HttpClientXLightWebEnginesArrayByte
import blueeyes.core.service.HttpClient
import blueeyes.demo.{BlueEyesDemo, Contact, ContactListFacade}
import java.util.concurrent.CountDownLatch
import blueeyes.util.Future
import net.lag.configgy.Configgy

object Benchmark extends ServerStart{

  private val threadsCount = 3
  private val contactsSize = 1000

  def main(args: Array[String]){
    startServer

    println("Execution Time: " + benchmark(ContactStream(threadsCount, contactsSize)))

    stopServer

    System.exit(0)
  }

  private def benchmark(streams: List[Stream[Contact]]) = {
    val start = System.currentTimeMillis

    runBenchmarkTasks(streams)

    val end   = System.currentTimeMillis

    (end - start)
  }

  private def runBenchmarkTasks(streams: List[Stream[Contact]]){
    val benchmarkTasks = streams.map(startBenchmarkTask(_))

    benchmarkTasks.foreach(_.taskCounDown.await)    
  }

  private def startBenchmarkTask(contactsStream: Stream[Contact]) = {
    val benchmarkTask = new BenchmarkTask(port, contactsStream)
    new Thread(benchmarkTask).start
    benchmarkTask
  }
}

class BenchmarkTask(val port: Int, val contactsStream: Stream[Contact]) extends Runnable with ContactListFacade with HttpClientXLightWebEnginesArrayByte{
  val taskCounDown  = new CountDownLatch(1)
  def run = {
    val counDown = processStream({contact: Contact => create(contact)})

    counDown.await

    taskCounDown.countDown
  }

  private def processStream[T](f: Contact => HttpClient[Array[Byte]] => Future[T]) = {
    val counDown      = new CountDownLatch(contactsStream.size)

    contactsStream.foreach(contact => {
      val future    = exec[T](f(contact))
      future.deliverTo(response =>{
        counDown.countDown
      })
      future.ifCanceled{v =>
        counDown.countDown
        v.foreach(_.printStackTrace)
      }
    })

    counDown
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
