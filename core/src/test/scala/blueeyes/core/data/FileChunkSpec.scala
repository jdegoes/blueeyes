package blueeyes.core.data

import org.specs2.mutable.Specification
import java.io.File
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.Await
import akka.util.Duration
import blueeyes.bkka.AkkaDefaults
import blueeyes.concurrent.test.FutureMatchers
import collection.mutable.ArrayBuilder.ofByte
import org.specs2.specification.{AfterExample, BeforeAfterExample}


trait Data extends AkkaDefaults {
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)
  val data     = List.fill(5)(List.fill[Byte](10)('0'))
  val chunk    = data.tail.foldLeft(new ByteMemoryChunk(data.head.toArray)){(chunk, data) => new ByteMemoryChunk(data.toArray, () => Some(Future[ByteChunk](chunk)))}
}

class FileSinkSpec extends Specification with Data with BeforeAfterExample with AkkaDefaults with FutureMatchers {
  override def is = args(sequential = true) ^ super.is

  "FileSink" should {
    "write data" in{
      FileSink(dataFile, chunk) must whenDelivered {
        (_: Unit) => {
          (dataFile.exists must_==(true)) and 
          (dataFile.length mustEqual(data.flatten.length))
        }
      }
    }

    "cancel result when write failed" in{
      val error  = new RuntimeException
      val result = FileSink(dataFile, new ByteMemoryChunk(data.head.toArray, () => throw error))

      result.failed must whenDelivered {
        (err: Throwable) => (err must_== error) and (dataFile.exists must_== true) and (dataFile.length must_== data.head.toArray.length)
      }
    }

    "cancel result when write getting next chunk failed" in{
      val error  = new RuntimeException
      val result = FileSink(dataFile, new ByteMemoryChunk(data.head.toArray, () => Some(Promise.failed[ByteChunk](error))))

      result.failed must whenDelivered {
        (err: Throwable) => (err must_== error) and (dataFile.exists must_== true) and (dataFile.length must_== data.head.toArray.length)
      }
    }

    "cancel writing when result is canceled" in{
      val promise = Promise[ByteChunk]()
      def next   = {
        import scala.actors.Actor.actor
        actor {
          Thread.sleep(2000)
          promise.success(new ByteMemoryChunk(data.head.toArray))
        }
        Some(promise)
      }

      val result = FileSink(dataFile, new ByteMemoryChunk(data.head.toArray, next _))
      val killed = new RuntimeException("killed")
      result.asInstanceOf[Promise[Unit]].failure(killed)
      
      promise.failed must whenDelivered {
        (err: Throwable) => (err must_== killed) and (dataFile.exists must_== true) and (dataFile.length must_== data.head.toArray.length)
      }
    }
  }

  protected def before = dataFile.delete

  protected def after = dataFile.delete
}

class FileSourceSpec extends Specification with Data with AfterExample with AkkaDefaults with FutureMatchers {

  "FileSource" should {
    "read all data" in{
      def readContent(chunk: ByteChunk, buffer: ofByte): ofByte = {
        buffer ++= chunk.data

        val next = chunk.next
        next match{
          case None =>  buffer
          case Some(x) => readContent(Await.result(x, Duration.Inf), buffer)
        }
      }

      val result = FileSink(dataFile, chunk)
      result must whenDelivered (be_==(()))

      val fileChunks = FileSource(dataFile)

      new String(readContent(chunk, new ofByte()).result) mustEqual(new String(data.flatten.toArray))
    }
  }

  protected def after = dataFile.delete
}
