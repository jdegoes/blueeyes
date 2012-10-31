package blueeyes.core.data

import blueeyes.bkka._

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import java.nio.ByteBuffer
import java.nio.channels._
import java.io.{OutputStream, FileOutputStream, RandomAccessFile, File}

import scalaz._

class FileSource(file: File, offset: Long, length: Long, chunkSize: Int = 8192) {
  def this(file: File) = this(file, 0, file.length())

  def read(implicit executor: ExecutionContext): ByteChunk = {
    implicit val M = new FutureMonad(executor)

    if (length < chunkSize) {
      val raf = new RandomAccessFile(file, "r")
      try {
        raf.seek(offset)
        val buffer = ByteBuffer.allocate(length.toInt)
        raf.getChannel().read(buffer)
        buffer.flip()
        Left(buffer)
      } finally {
        raf.close()
      }
    } else {
      val raf = new RandomAccessFile(file, "r")
      Right(
        StreamT.unfoldM[Future, ByteBuffer, Option[(Long, FileChannel)]](Some((0L, raf.getChannel()))) { 
          case Some((consumed, channel)) => 
            Future {
              val buffer = ByteBuffer.allocate(chunkSize)
              val read = channel.read(buffer) 
              if (read == -1) {
                None
              } else {
                buffer.flip()
                Some((buffer, Some((consumed + read, channel))))
              }
            }

          case None =>
            Future { raf.close(); None }
        }
      )
    }
  }
}

class FileSink(file: File) {
  import FileSink.KillSwitch

  def write(chunk: ByteChunk)(implicit executor: ExecutionContext): (Option[KillSwitch], Future[Unit]) = {
    implicit val M = new FutureMonad(executor)

    val killed = new java.util.concurrent.atomic.AtomicReference[Option[Throwable]](None)
    def writeStream(stream: StreamT[Future, ByteBuffer], out: FileChannel): Future[Unit] = {
      stream.uncons flatMap {
        case Some((head, tail)) =>
          killed.get() match {
            case Some(error) =>
              out.close()
              throw error

            case None =>
              out.write(head)
              writeStream(tail, out)
          }

        case None =>
          Future(out.close())
      }
    }

    file.createNewFile()
    chunk match {
      case Left(buffer) =>
        None -> Future {
          val out = new FileOutputStream(file)
          try {
            out.getChannel.write(buffer)
          } finally {
            out.close()
          }
        }

      case Right(stream) =>
        val killSwitch = new KillSwitch {
          def kill(killWith: Throwable) = killed.compareAndSet(None, Some(killWith))
        }

        Some(killSwitch) -> writeStream(stream, new FileOutputStream(file).getChannel())
    }
  }
}

object FileSink {
  trait KillSwitch {
    def kill(killWith: Throwable): Unit
  }

  def write(file: File, chunk: ByteChunk)(implicit executor: ExecutionContext) = new FileSink(file).write(chunk)
}
