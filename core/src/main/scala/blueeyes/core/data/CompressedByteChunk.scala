package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.Promise
import blueeyes.bkka.AkkaDefaults
import java.io.{OutputStream, ByteArrayOutputStream}
import java.util.zip.{Deflater, DeflaterOutputStream, GZIPOutputStream}

trait CompressedByteChunk extends AkkaDefaults {
  import CompressedByteChunk._

  def apply(dataChunk: ByteChunk): ByteChunk

  protected def create(dataChunk: ByteChunk)(compressStreamFactory: ByteArrayOutputStream => FinishableOutputStream): ByteChunk = {
    val dataStream     = new ByteArrayOutputStream()
    val compressStream = compressStreamFactory(dataStream)

    val (compressed, nextChunk) = compressChunk(dataChunk, compressStream, dataStream)
    dataStream.reset()

    buildChunk(compressed, nextChunk, compressStream, dataStream)
  }

  protected def buildChunk(data: Array[Byte], nextChunk: Option[Future[ByteChunk]], compressStream: FinishableOutputStream, dataStream: ByteArrayOutputStream) : ByteChunk = {
    lazy val next: Option[Future[ByteChunk]] = nextChunk map { nextDataFuture =>
      val promise = Promise[ByteChunk]()
      scheduleCompressChunk(nextDataFuture, promise)
      promise
    }

    def scheduleCompressChunk(nextDataFuture: Future[ByteChunk], promise: Promise[ByteChunk]) {
      nextDataFuture.foreach(chunk => compress(chunk, promise))
      nextDataFuture onFailure { case error =>
        compressStream.close()
        promise.failure(error)
      }
      //promise.ifCanceled(error => nextDataFuture.cancel(error))
    }

    def compress(chunk: ByteChunk, promise: Promise[ByteChunk]){
      val (compressed, nextChunk) = compressChunk(chunk, compressStream, dataStream)
      if (!compressed.isEmpty){
        promise.success(buildChunk(compressed, nextChunk, compressStream, dataStream))
        dataStream.reset()
      } else {
        nextChunk.foreach(nextChunkFuture => scheduleCompressChunk(nextChunkFuture, promise))
      }
    }

    Chunk(data, next)
  }

  private def compressChunk(chunk: ByteChunk, compressStream: FinishableOutputStream, dataStream: ByteArrayOutputStream) = {
    compressStream.write(chunk.data, 0, chunk.data.size)

    val nextChunk = chunk.next.orElse {
      compressStream.finish()
      compressStream.close()
      None
    }

    (dataStream.toByteArray, nextChunk)
  }
}

object CompressedByteChunk {
  type FinishableOutputStream = OutputStream { def finish(): Unit }
}

object GZIPByteChunk extends CompressedByteChunk {
  def apply(dataChunk: ByteChunk) = create(dataChunk) { new GZIPOutputStream(_) }
}

object ZLIBByteChunk extends CompressedByteChunk {
  def apply(dataChunk: ByteChunk): ByteChunk = apply(dataChunk, Deflater.BEST_SPEED)
  def apply(dataChunk: ByteChunk, level: Int): ByteChunk = create(dataChunk) { dataStream => new DeflaterOutputStream(dataStream, new Deflater(level)) }
}
