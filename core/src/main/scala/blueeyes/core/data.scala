package blueeyes.core

import blueeyes.util.metrics.DataSize

import blueeyes.bkka._
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import java.nio.ByteBuffer

import scalaz._

package object data {
  type Chunk[A] = Either[A, StreamT[Future, A]]
  type ByteChunk = Chunk[ByteBuffer]

  object Chunk {
    implicit def pointed(implicit M: Monad[Future]): Pointed[Chunk] = new Pointed[Chunk] {
      def point[A](a: => A) = Left(a)
      def map[A, B](c: Chunk[A])(f: A => B): Chunk[B] = c match {
        case Left(a) => Left(f(a))
        case Right(stream) => Right(stream.map(f))
      }
    }
  }

  object ByteChunk {
    val defaultChunkSize = 8192

    def apply(data: Array[Byte]) = Left(ByteBuffer.wrap(data))
    def aggregate(chunk: ByteChunk, chunkSize: Int)(implicit executor: ExecutionContext): ByteChunk = {
      implicit val M = new FutureMonad(executor)

      @inline def alloc = ByteBuffer.allocate(chunkSize)

      def fill(buffer: ByteBuffer, head: ByteBuffer, tail: StreamT[Future, ByteBuffer]): Future[(ByteBuffer, StreamT[Future, ByteBuffer])] = {
        if (head.remaining > buffer.remaining) {
          Future {
            val limit = head.limit
            // set the limit to the max that can be read into the current buffer
            val newLimit = head.position + buffer.remaining
            head.limit(newLimit)
            buffer.put(head)
            // reset the limit to its original position
            head.limit(limit)

            (buffer, head :: tail)
          }
        } else {
          tail.uncons flatMap {
            case Some((head0, tail0)) => 
              fill(buffer.put(head), head0, tail0)

            case None => 
              buffer.put(head)
              Promise.successful((buffer, StreamT.empty[Future, ByteBuffer]))
          }
        }
      }
      
      chunk.right map { stream =>
        StreamT.unfoldM[Future, ByteBuffer, (ByteBuffer, StreamT[Future, ByteBuffer])]((alloc, stream)) {
          case (acc, stream) =>
            stream.uncons flatMap {
              case Some((head, tail)) => 
                fill(acc, head, tail) map { 
                  case (filled, remainder) => 
                    filled.flip()
                    Some((filled, (alloc, remainder)))
                }

              case None =>
                Promise.successful(None)
            }
        }
      }
    }

    def force(s: ByteChunk)(implicit exeuctor: ExecutionContext): Future[Either[ByteBuffer, (Vector[ByteBuffer], Int)]] = {
      s match {
        case Right(stream) =>
          import blueeyes.bkka.AkkaTypeClasses._
          stream.foldLeft((Vector.empty[ByteBuffer], 0)) { 
            case ((acc, size), buffer) => (acc :+ buffer, size + buffer.remaining)
          } map { 
            case (buffers, size) =>
              if (buffers.size == 1) {
                Left(buffers.head)
              } else {
                Right((buffers, size))
              }
          }

        case Left(buffer) => 
          Future(Left(buffer)) // either right type must match
      }
    }

    def forceByteArray(s: ByteChunk)(implicit exeuctor: ExecutionContext): Future[Array[Byte]] = {
      ByteChunk.force(s) map {
        case Left(buffer) =>
          val resultArray = new Array[Byte](buffer.remaining)
          buffer.get(resultArray)
          resultArray

        case Right((acc, size)) =>
          val resultArray = new Array[Byte](size)
          val resultBuf = ByteBuffer.wrap(resultArray)
          acc foreach { resultBuf put _ }
          resultArray
      }
    }

    def forceEagerChunk(s: ByteChunk)(implicit executor: ExecutionContext): Future[ByteChunk] = {
      forceByteArray(s) map { bytes => Left(ByteBuffer.wrap(bytes)) }
    }
  }
}
