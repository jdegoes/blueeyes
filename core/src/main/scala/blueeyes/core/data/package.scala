package blueeyes.core

import scalaz.StreamT
import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import java.nio.ByteBuffer

package object data {
  type ByteChunk = Either[ByteBuffer, StreamT[Future, ByteBuffer]]

  object ByteChunk {
    def force(s: ByteChunk)(implicit ctx: ExecutionContext): Future[Either[ByteBuffer, (Vector[ByteBuffer], Int)]] = {
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

    def forceByteArray(s: ByteChunk)(implicit ctx: ExecutionContext): Future[Array[Byte]] = {
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

    def aggregate(s: ByteChunk)(implicit ctx: ExecutionContext): Future[ByteChunk] = {
      forceByteArray(s) map { bytes => Left(ByteBuffer.wrap(bytes)) }
    }
  }
}
