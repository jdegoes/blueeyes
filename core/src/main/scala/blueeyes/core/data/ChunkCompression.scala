package blueeyes.core.data

import blueeyes.bkka._
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import java.io._
import java.nio._
import java.nio.channels._
import java.util.zip.{Deflater, DeflaterOutputStream, GZIPOutputStream}

import scalaz._

abstract class ChunkCompression(implicit val executor: ExecutionContext) {
  protected implicit val M = new FutureMonad(executor)

  def compress(dataChunk: ByteChunk, chunkSize: Int = 8192): ByteChunk = {
    dataChunk match {
      case Left(data) => Left(compress(data))
      case Right(stream) =>
        val inPipe = new PipedInputStream()
        val inChannel = Channels.newChannel(new BufferedInputStream(inPipe))
   
        writeCompressed(stream, new PipedOutputStream(inPipe))
        Right(
          StreamT.unfoldM[Future, ByteBuffer, Option[ReadableByteChannel]](Some(inChannel)) { 
            case Some(in) => 
              Future {
                val buffer = ByteBuffer.allocate(chunkSize)
                val read = in.read(buffer) 
                buffer.flip()
                if (read == -1) Some((buffer, None)) else Some((buffer, Some(in)))
              }

            case None =>
              Promise.successful(None)
          }
        )
    }
  }

  protected def channel(out: OutputStream): WritableByteChannel

  protected def compress(buf: ByteBuffer): ByteBuffer = {
    val byteStream = new ByteArrayOutputStream()
    val c = channel(byteStream)
    c.write(buf)
    c.close()
    ByteBuffer.wrap(byteStream.toByteArray)
  }

  protected def writeCompressed(stream: StreamT[Future, ByteBuffer], out: OutputStream): Future[Unit] = {
    def writeChannel(stream: StreamT[Future, ByteBuffer], c: WritableByteChannel): Future[Unit] = {
      stream.uncons flatMap {
        case Some((buffer, tail)) =>
          c.write(buffer)
          writeChannel(tail, c)
        case None => 
          Future(c.close())
      }
    }

    writeChannel(stream, channel(out))
  }
}

object ChunkCompression {
  def gzip(implicit ctx: ExecutionContext): ChunkCompression = new GZIPChunkCompression(ctx)
  def zlib(level: Option[Int] = None)(implicit ctx: ExecutionContext): ChunkCompression = {
    new ZLIBChunkCompression(level.getOrElse(Deflater.BEST_SPEED), ctx)
  }
}

class GZIPChunkCompression(ctx: ExecutionContext) extends ChunkCompression()(ctx) {
  protected def channel(out: OutputStream): WritableByteChannel = {
    Channels.newChannel(new GZIPOutputStream(out))
  }
}

class ZLIBChunkCompression(level: Int, ctx: ExecutionContext) extends ChunkCompression()(ctx) {
  protected def channel(out: OutputStream): WritableByteChannel = {
    Channels.newChannel(new DeflaterOutputStream(out, new Deflater(level)))
  }
}
