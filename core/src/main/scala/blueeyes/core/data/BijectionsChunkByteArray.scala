package blueeyes.core.data

import blueeyes.bkka._
import akka.dispatch.Future
import akka.dispatch.Await
import akka.dispatch.ExecutionContext
import akka.util.Timeout

import java.nio.ByteBuffer
import scalaz._
import scalaz.StreamT._
import scalaz.syntax.monad._

trait BijectionsChunkByteArray {
  implicit def arrayByteToChunk(implicit context: ExecutionContext) = {
    new Bijection[Future[Array[Byte]], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(context)

      def apply(t: Future[Array[Byte]]): ByteChunk = {
        Right(t.map(ByteBuffer.wrap _).liftM[StreamT])
      }

      def unapply(s: ByteChunk): Future[Array[Byte]] = {
        ByteChunk.forceByteArray(s)
      }
    }
  }
}


object BijectionsChunkByteArray extends BijectionsChunkByteArray 
