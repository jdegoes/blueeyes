package blueeyes.core.data

import blueeyes.bkka._

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import java.nio.ByteBuffer
import java.nio.channels._
import java.io.{OutputStream, FileOutputStream, RandomAccessFile, File}

import scalaz._

import scala.math.min

object FileSource {
  def apply(file: File): FileSource = {
    val len = file.length
    if (len > Int.MaxValue)
      throw new IllegalArgumentException("file is too large (%d)" format len)
    new FileSource(file, 0, len.toInt)
  }

  def apply(file: File, offset: Long, length: Int, chunkSize: Int = 8192): FileSource =
    new FileSource(file, offset, length, chunkSize)
}

class FileSource(file: File, offset: Long, length: Int, chunkSize: Int = 8192) {
  //def this(file: File) = if (this(file, 0, file.length())

  def read(implicit executor: ExecutionContext): ByteChunk = {
    implicit val M = new FutureMonad(executor)

    val raf = new RandomAccessFile(file, "r")
    try {
      raf.seek(offset)

      def readUntil(arr: Array[Byte], size: Int): Int = {
        var offset = 0
        var left = size
        while (true) {
          val n = raf.read(arr, offset, left)
          if (n == -1) return size - left
          offset += n
          left -= n
          if (left == 0) return size
        }
        return -1 // impossible
      }

      def makeArray(size: Int): Array[Byte] = {
        val arr = new Array[Byte](size)
        val n = readUntil(arr, size)
        if (n == length) {
          arr
        } else if (n > 0) {
          val arr2 = new Array[Byte](n)
          System.arraycopy(arr, 0, arr2, 0, n)
          arr2
        } else {
          new Array[Byte](0)
        }
      }

      def makeStreamT(left: Int): StreamT[Future, Array[Byte]] = {
        val size = min(chunkSize, left)
        val arr = makeArray(size)
        if (arr.length < size || size == left) {
          arr :: StreamT.empty[Future, Array[Byte]]
        } else {
          arr :: makeStreamT(left - size)
        }
      }

      if (length < chunkSize)
        Left(makeArray(length))
      else
        Right(makeStreamT(length))
    } finally {
      raf.close()
    }
  }
}

class FileSink(file: File) {
  import FileSink.KillSwitch

  def write(chunk: ByteChunk)(implicit executor: ExecutionContext): (Option[KillSwitch], Future[Unit]) = {
    implicit val M = new FutureMonad(executor)

    val killed = new java.util.concurrent.atomic.AtomicReference[Option[Throwable]](None)

    def writeStream(stream: StreamT[Future, Array[Byte]], out: FileChannel): Future[Unit] = {
      stream.uncons flatMap {
        case Some((head, tail)) =>
          killed.get() match {
            case Some(error) =>
              out.close()
              throw error

            case None =>
              out.write(ByteBuffer.wrap(head))
              writeStream(tail, out)
          }

        case None =>
          Future(out.close())
      }
    }

    file.createNewFile()

    chunk match {
      case Left(bytes) =>
        None -> Future {
          val out = new FileOutputStream(file)
          try {
            out.getChannel.write(ByteBuffer.wrap(bytes))
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
