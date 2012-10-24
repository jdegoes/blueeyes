package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import java.io.{OutputStream, ByteArrayOutputStream}
import java.util.zip.{Deflater, DeflaterOutputStream, GZIPOutputStream}

trait ChunkCompression extends Compression[Chunk[Array[Byte]]] {
  import ChunkCompression._

  def apply(dataChunk: Chunk[Array[Byte]]): Chunk[Array[Byte]]

  protected def create(dataChunk: Chunk[Array[Byte]])(compressStreamFactory: ByteArrayOutputStream => FinishableOutputStream)(implicit ctx: ExecutionContext): Chunk[Array[Byte]] = {
    val dataStream     = new ByteArrayOutputStream()
    val compressStream = compressStreamFactory(dataStream)

    val (compressed, nextChunk) = compressChunk(dataChunk, compressStream, dataStream)
    dataStream.reset()

    buildChunk(compressed, nextChunk, compressStream, dataStream)
  }

  protected def buildChunk(data: Array[Byte], nextChunk: Option[Future[Chunk[Array[Byte]]]], compressStream: FinishableOutputStream, dataStream: ByteArrayOutputStream)(implicit ctx: ExecutionContext) : Chunk[Array[Byte]] = {
    lazy val next: Option[Future[Chunk[Array[Byte]]]] = nextChunk map { nextDataFuture =>
      val promise = Promise[Chunk[Array[Byte]]]()
      scheduleCompressChunk(nextDataFuture, promise)
      promise
    }

    def scheduleCompressChunk(nextDataFuture: Future[Chunk[Array[Byte]]], promise: Promise[Chunk[Array[Byte]]]) {
      nextDataFuture.foreach(chunk => compress(chunk, promise))
      nextDataFuture onFailure { case error =>
        compressStream.close()
        promise.failure(error)
      }
      //promise.ifCanceled(error => nextDataFuture.cancel(error))
    }

    def compress(chunk: Chunk[Array[Byte]], promise: Promise[Chunk[Array[Byte]]]){
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

  private def compressChunk(chunk: Chunk[Array[Byte]], compressStream: FinishableOutputStream, dataStream: ByteArrayOutputStream) = {
    compressStream.write(chunk.data, 0, chunk.data.size)

    val nextChunk = chunk.next.orElse {
      compressStream.finish()
      compressStream.close()
      None
    }

    (dataStream.toByteArray, nextChunk)
  }
}

object ChunkCompression {
  type FinishableOutputStream = OutputStream { def finish(): Unit }
}

class GZIPChunkCompression(implicit ctx: ExecutionContext) extends ChunkCompression {
  def apply(dataChunk: Chunk[Array[Byte]]) = create(dataChunk) { new GZIPOutputStream(_) }
}

class ZLIBChunkCompression(implicit ctx: ExecutionContext) extends ChunkCompression {
  def apply(dataChunk: Chunk[Array[Byte]]): Chunk[Array[Byte]] = apply(dataChunk, Deflater.BEST_SPEED)
  def apply(dataChunk: Chunk[Array[Byte]], level: Int): Chunk[Array[Byte]] = create(dataChunk) { dataStream => new DeflaterOutputStream(dataStream, new Deflater(level)) }
}
