package blueeyes.core.data

import org.specs.Specification
import java.io.File
import blueeyes.concurrent.{Future, FutureDeliveryStrategySequential}
import collection.mutable.ArrayBuilder.ofByte

class FileSinkSpec extends Specification with Data{

  "FileSink" should {
    "write data" in{
      dataFile.delete
      val result = FileSink(dataFile, chunk)

      result.value must eventually (beSome(()))
      dataFile.exists must be (true)
      dataFile.length mustEqual(data.flatten.length)
    }
    "cancel result when write failed" in{

      dataFile.delete
      val error  = new RuntimeException
      val result = FileSink(dataFile, new ByteMemoryChunk(data.head.toArray, () => throw error))

      result.isCanceled must eventually (be(true))
      result.error must beSome(error)
      dataFile.exists must be (true)
      dataFile.length mustEqual(data.head.toArray.length)
    }
    "cancel result when write getting next chunk failed" in{
      dataFile.delete

      val error  = new RuntimeException
      val result = FileSink(dataFile, new ByteMemoryChunk(data.head.toArray, () => Some(Future.dead[ByteChunk](error))))

      result.isCanceled must eventually (be(true))
      result.error must beSome(error)
      dataFile.exists must be (true)
      dataFile.length mustEqual(data.head.toArray.length)
    }
    "cancel writing when result is canceled" in{
      dataFile.delete

      val nextChunk = new Future[ByteChunk]()
      def next   = {
        import scala.actors.Actor.actor
        actor {
          Thread.sleep(2000)
          nextChunk.deliver(new ByteMemoryChunk(data.head.toArray))
        }
        Some(nextChunk)
      }
      val result = FileSink(dataFile, new ByteMemoryChunk(data.head.toArray, next _))
      result.cancel
      nextChunk.isCanceled must eventually (be(true))

      dataFile.exists must be (true)
      dataFile.length mustEqual(data.head.toArray.length)
    }
    doLast{
      dataFile.delete
    }
  }
}

class FileSourceSpec extends Specification with Data{

  "FileSource" should {
    "read all data" in{
      val result = FileSink(dataFile, chunk)
      result.value must eventually (beSome(()))

      val fileChunks = FileSource(dataFile)

      new String(readContent(chunk, new ofByte()).result) mustEqual(new String(data.flatten.toArray))
    }
    def readContent(chunk: ByteChunk, buffer: ofByte): ofByte = {
      buffer ++= chunk.data

      val next = chunk.next
      next match{
        case None =>  buffer
        case Some(x) => readContent(x.value.get, buffer)
      }
    }

    doLast{
      dataFile.delete
    }
  }
}

trait Data extends FutureDeliveryStrategySequential{
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)
  val data     = List.fill(5)(List.fill[Byte](10)('0'))
  val chunk    = data.tail.foldLeft(new ByteMemoryChunk(data.head.toArray)){(chunk, data) => new ByteMemoryChunk(data.toArray, () => Some(Future[ByteChunk](chunk)))}
}