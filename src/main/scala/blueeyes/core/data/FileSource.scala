package blueeyes.core.data

import blueeyes.bkka.AkkaDefaults
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.util.Timeout
import java.io.{OutputStream, FileOutputStream, RandomAccessFile, File}

class FileSource(file: File, private[FileSource] var offset: Long, length: Long, chunkSize: Int = 8192) extends AkkaDefaults {
  def this(file: File) = this(file, 0, file.length())

  private val accessFile = new RandomAccessFile(file, "r")
  private val endOffset  = offset + length

  // todo how it can be cancelled (problem is that Future is done and cannot be cancelled)
  def apply: Option[Future[ByteChunk]] = {
    if (offset < endOffset){
      try {
        val size = chunkSize.toLong min (endOffset - offset)
        val chunk = new Array[Byte](size.toInt)

        accessFile.readFully(chunk)

        offset = offset + size

        Some(Future(new MemoryChunk(chunk, apply _)))
      } catch {
        case th: Throwable =>
          accessFile.close()
          throw th
      }
    } else {
      accessFile.close()
      None
    }
  }
}

object FileSource {
  import akka.dispatch.Await
  /**
   * Read the given file into a bytechunk. By default, we wait forever
   * for IO, but you can reduce this if desired.
   */
  def apply(file: File, timeout: akka.util.Timeout = Timeout.never): Option[ByteChunk] = new FileSource(file).apply.map(future => Await.result(future, timeout.duration))
}

class FileSink(file: File) extends AkkaDefaults {
  private var output: Option[OutputStream] = None

  def apply(chunk: ByteChunk): Future[Unit] = {
    val promise  = Promise[Unit]()
    try {
      file.createNewFile()
      output = Some(new FileOutputStream(file))
      write(chunk, promise)
    } catch {
      case th: Throwable => cancel(promise, th)
    }

    promise
  }

  private def write(chunk: ByteChunk, promise: Promise[Unit]) {
    try {
      output.foreach{ stream =>
        stream.write(chunk.data)
        stream.flush()
      }

      chunk.next match{
        case Some(future) => {
          future  foreach   { write(_: ByteChunk, promise) } 
          future  onFailure { case th => cancel(promise, th) }
          promise onFailure { case th => future.asInstanceOf[Promise[ByteChunk]].failure(th) }  
        }

        case None =>
          close
          promise.success(())
      }
    } catch {
      case th: Throwable => cancel(promise, th)
    }
  }

  private def cancel(promise: Promise[Unit], error: Throwable){
    promise.failure(error)
    close
  }

  private def close = {
    output.foreach(_.close)
    output = None
  }
}

object FileSink{
  def apply(file: File, chunk: ByteChunk) = new FileSink(file).apply(chunk)
}
