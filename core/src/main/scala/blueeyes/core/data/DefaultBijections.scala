package blueeyes.core.data

import blueeyes.bkka._
import blueeyes.json._
import blueeyes.json.JParser

import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import java.nio.ByteBuffer

import scalaz._
import scalaz.syntax.monad._
import scala.xml.NodeSeq
import scala.xml.XML

trait DefaultBijections {
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream, PrintStream}
  import java.nio.ByteBuffer

  implicit val JValueToByteArray = new Bijection[JValue, Array[Byte]]{
    def apply(jv: JValue) = {
      jv.renderCompact.getBytes("UTF-8")
    }

    def unapply(arr: Array[Byte]) = {
      val bb = ByteBuffer.wrap(arr)
      val r = JParser.parseFromByteBuffer(bb)
      r.valueOr(e => throw e)
    }
  }

  implicit val XMLToByteArray = new Bijection[NodeSeq, Array[Byte]] {
    def apply(s: NodeSeq)       = s.toString.getBytes("UTF-8")
    def unapply(t: Array[Byte]) = XML.loadString(new String(t, "UTF-8"))
  }

  implicit val ByteArrayToString = new Bijection[Array[Byte], String] {
    def apply(t: Array[Byte]): String   = new String(t, "UTF-8")
    def unapply(s: String): Array[Byte] = s.getBytes("UTF-8")
  }

  implicit val ByteArrayToJValue    = JValueToByteArray.inverse
  implicit val ByteArrayToXML       = XMLToByteArray.inverse
  implicit val StringToByteArray    = ByteArrayToString.inverse

  implicit def byteArrayToChunk(a: Array[Byte]): ByteChunk = Left(ByteBuffer.wrap(a))

  implicit def futureByteArrayToChunk(implicit executor: ExecutionContext): Bijection[Future[Array[Byte]], ByteChunk] = {
    new Bijection[Future[Array[Byte]], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(executor)

      def apply(t: Future[Array[Byte]]): ByteChunk = {
        Right(t.map(ByteBuffer.wrap _).liftM[StreamT])
      }

      def unapply(s: ByteChunk): Future[Array[Byte]] = {
        ByteChunk.forceByteArray(s)
      }
    }
  }

  implicit def chunkToFutureByteArray(implicit executor: ExecutionContext) = futureByteArrayToChunk.inverse

  implicit def stringToChunk(s: String): ByteChunk = Left(ByteBuffer.wrap(s.getBytes("UTF-8")))

  implicit def futureStringToChunk(implicit executor: ExecutionContext): Bijection[Future[String], ByteChunk] = {
    new Bijection[Future[String], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(executor)

      def apply(t: Future[String]): ByteChunk = {
        Right(t.map(s => ByteBuffer.wrap(s.getBytes("UTF-8"))).liftM[StreamT])
      }

      def unapply(s: ByteChunk): Future[String] = {
        ByteChunk.forceByteArray(s).map(bytes => new String(bytes, "UTF-8"))
      }
    }
  }

  implicit def chunkToFutureString(implicit executor: ExecutionContext) = futureStringToChunk.inverse

  implicit def jvalueToChunk(jv: JValue) = Left(ByteBuffer.wrap(JValueToByteArray(jv)))

  implicit def futureJValueToChunk(implicit executor: ExecutionContext): Bijection[Future[JValue], ByteChunk] = {
    new Bijection[Future[JValue], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(executor)

      def apply(t: Future[JValue]) = {
        Right(t.map(jv => ByteBuffer.wrap(JValueToByteArray(jv))).liftM[StreamT])
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

  implicit def chunkToFutureJValue(implicit executor: ExecutionContext) = futureJValueToChunk.inverse
}

object DefaultBijections extends DefaultBijections
