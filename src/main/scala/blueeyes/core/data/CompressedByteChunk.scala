package blueeyes.core.data

import blueeyes.concurrent.Future
import java.io.{OutputStream, ByteArrayOutputStream}
import java.util.zip.{Deflater, DeflaterOutputStream, GZIPOutputStream}

trait CompressedByteChunk{
  def apply(dataChunk: ByteChunk): ByteChunk

  protected def create(dataChunk: ByteChunk, compressStreamFactory: (ByteArrayOutputStream) => OutputStream {def finish()}): ByteChunk = {
    val dataStream     = new ByteArrayOutputStream()
    val compressStream = compressStreamFactory(dataStream)

    val (compressed, nextChunk) = compressChunk(dataChunk, compressStream, dataStream)
    dataStream.reset()

    new CompressedByteChunk(compressed, nextChunk, compressStream, dataStream)
  }

  private[CompressedByteChunk] def compressChunk(chunk: ByteChunk, compressStream: OutputStream {def finish()}, dataStream: ByteArrayOutputStream) = {
    compressStream.write(chunk.data, 0, chunk.data.size)

    val nextChunk  = chunk.next
    nextChunk.orElse{
      compressStream.finish()
      compressStream.close()
      None
    }
    (dataStream.toByteArray, nextChunk)
  }

  class CompressedByteChunk private[data](val data: Array[Byte], nextChunk: Option[Future[ByteChunk]], compressStream: OutputStream {def finish()}, dataStream: ByteArrayOutputStream) extends ByteChunk{
    lazy val next: Option[Future[ByteChunk]] = nextChunk.map{nextDataFuture =>
      val future = new Future[ByteChunk]()
      scheduleCompressChunk(nextDataFuture, future)
      future
    }

    private def scheduleCompressChunk(nextDataFuture: Future[ByteChunk], future: Future[ByteChunk]){
      nextDataFuture.deliverTo(chunk => compress(chunk, future))
      nextDataFuture.ifCanceled{error =>
        compressStream.close()
        future.cancel(error)
      }
      future.ifCanceled(error => nextDataFuture.cancel(error))
    }

    private def compress(chunk: ByteChunk, future: Future[ByteChunk]){
      val (compressed, nextChunk) = compressChunk(chunk, compressStream, dataStream)
      if (!compressed.isEmpty){
        future.deliver(new CompressedByteChunk(compressed, nextChunk, compressStream, dataStream))
        dataStream.reset()
      }
      else {
        nextChunk.foreach(nextChunkFuture => scheduleCompressChunk(nextChunkFuture, future))
      }
    }
  }
}

object GZIPByteChunk extends CompressedByteChunk{
  def apply(dataChunk: ByteChunk) = create(dataChunk, (dataStream) => new GZIPOutputStream(dataStream))
}

object ZLIBByteChunk extends CompressedByteChunk{
  def apply(dataChunk: ByteChunk) = create(dataChunk, {dataStream => new DeflaterOutputStream(dataStream, new Deflater(Deflater.BEST_SPEED))})
}