package blueeyes.core.service.engines

import org.jboss.netty.handler.stream.ChunkedInput
import org.jboss.netty.handler.stream.ChunkedWriteHandler
import org.jboss.netty.channel.Channel
import net.lag.logging.Logger
import blueeyes.concurrent.{Future, ReadWriteLock}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http.{DefaultHttpChunkTrailer, DefaultHttpChunk}
import blueeyes.core.data._

private[engines] class NettyChunkedInput(chunk: ByteChunk, channel: Channel) extends ChunkedInput{

  private val log   = Logger.get
  private var done  = false

  private val lock = new ReadWriteLock{}
  private var nextChunkFuture: Future[ByteChunk] = _

  setNextChunkFuture(Future.sync(chunk))

  def close() {nextChunkFuture.cancel()}

  def isEndOfInput = !hasNextChunk

  def nextChunk = {
    nextChunkFuture.value.map{chunk =>
      val data = chunk.data
      if (!data.isEmpty) new DefaultHttpChunk(ChannelBuffers.wrappedBuffer(data)) else new DefaultHttpChunkTrailer()
    }.orElse{
      nextChunkFuture.deliverTo{nextChunk =>
        channel.getPipeline.get(classOf[ChunkedWriteHandler]).resumeTransfer()
      }
      None
    }.orNull
  }

  def hasNextChunk = {
    nextChunkFuture.value.map{chunk =>
      chunk.next match{
        case Some(future)     => setNextChunkFuture(future)
          true
        case None if (!done)  => {
          setNextChunkFuture(Future.sync[ByteChunk](new MemoryChunk(Array[Byte]())))
          done = true
          true
        }
        case _ => false
      }
    }.getOrElse(true)
  }

  private def setNextChunkFuture(future: Future[ByteChunk]){
    future.trap{errors: List[Throwable] =>
      errors.foreach(error => log.warning(error, "An exception was raised by NettyChunkedInput."))
      errors match{
        case x :: xs => throw x
        case _ =>
      }
    }
    lock.writeLock{
      nextChunkFuture = future
    }
  }
}