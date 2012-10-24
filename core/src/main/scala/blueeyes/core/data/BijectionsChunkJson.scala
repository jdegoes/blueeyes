package blueeyes.core.data

import blueeyes.bkka._
import blueeyes.json._
import blueeyes.json.JParser

import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream, PrintStream}
import java.nio.ByteBuffer
import scalaz._
import scalaz.StreamT._
import scalaz.syntax.monad._

trait BijectionsChunkJson {
  private def renderBytes(jv: JValue): ByteBuffer = {
    ByteBuffer.wrap(jv.renderCompact.getBytes("UTF-8"))
  }

  implicit def jvalueToChunk(jv: JValue) = Left(renderBytes(jv))

  implicit def futureJValueToChunk(implicit context: ExecutionContext) = {
    new Bijection[Future[JValue], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(context)

      def apply(t: Future[JValue]) = {
        Right(t.map(renderBytes).liftM[StreamT])
      }

      def unapply(s: ByteChunk) = {
        ByteChunk.force(s) map {
          case Left(buffer) =>
            JParser.parseFromByteBuffer(buffer) | JUndefined
          
          case Right((acc, size)) => 
            val resultArray = new Array[Byte](size)
            val resultBuf = ByteBuffer.wrap(resultArray)
            acc foreach { resultBuf put _ }
            resultBuf.flip()
            JParser.parseFromByteBuffer(resultBuf) | JUndefined
        }
      }
    }
  }
}

object BijectionsChunkJson extends BijectionsChunkJson

