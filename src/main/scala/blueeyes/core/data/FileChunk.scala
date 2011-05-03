package blueeyes.core.data

import scala.math.min
import blueeyes.concurrent.{FutureDeliveryStrategy, Future}
import java.io.{OutputStream, FileOutputStream, RandomAccessFile, File}

class FileSource(file: File, private[FileSource] var offset: Long, length: Long, chunkSize: Int = 8192)(implicit deliveryStrategy: FutureDeliveryStrategy){
  def this(file: File)(implicit deliveryStrategy: FutureDeliveryStrategy) = this(file, 0, file.length())

  private val accessFile = new RandomAccessFile(file, "r")
  private val endOffset  = offset + length

  // todo how it can be cancelled (problem is that Future is done and cannot be cancelled)
  def apply: Option[Future[ByteChunk]] = {
    if (offset < endOffset){
      try {
        val size = min(chunkSize, endOffset - offset)
        val chunk = new Array[Byte](size.toInt)

        accessFile.readFully(chunk)

        offset = offset + size

        Some(Future(new MemoryChunk(chunk, apply _)))
      }
      catch {
        case th: Throwable =>
          accessFile.close
          throw th
      }
    }
    else {
      accessFile.close
      None
    }
  }
}

object FileSource{
  def apply(file: File)(implicit deliveryStrategy: FutureDeliveryStrategy): Option[ByteChunk] = new FileSource(file).apply.map(future => future.value.get)
}

class FileSink(file: File){
  private var output: Option[OutputStream] = None
  def apply(chunk: ByteChunk)(implicit deliveryStrategy: FutureDeliveryStrategy): Future[Unit] = {
    val result  = new Future[Unit]()
    try {
      file.createNewFile()

      output = Some(new FileOutputStream(file))

      write(chunk, result)
    }
    catch {
      case th: Throwable => cancel(result, Some(th))
    }

    result
  }

  private def write(chunk: ByteChunk, result: Future[Unit]){
    try {
      output.foreach{ stream =>
        stream.write(chunk.data)
        stream.flush()
      }

      chunk.next match{
        case Some(future) => {
          future.deliverTo(write(_, result))
          future.ifCanceled(th => cancel(result, th))
          result.ifCanceled(th => future.cancel(th))
        }
        case None =>
          close
          result.deliver(())
      }
    }
    catch {
      case th: Throwable => cancel(result, Some(th))
    }
  }

  private def cancel(result: Future[Unit], th: Option[Throwable]){
    result.cancel(th)
    close
  }

  private def close = {
    output.foreach(_.close)
    output = None
  }
}

object FileSink{
  def apply(file: File, chunk: ByteChunk)(implicit deliveryStrategy: FutureDeliveryStrategy) = new FileSink(file).apply(chunk)
}