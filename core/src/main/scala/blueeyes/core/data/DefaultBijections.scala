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
  implicit def byteArrayToChunk(a: Array[Byte]): ByteChunk = Left(a)

  implicit def futureByteArrayToChunk(implicit executor: ExecutionContext): Bijection[Future[Array[Byte]], ByteChunk] = {
    new Bijection[Future[Array[Byte]], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(executor)
      def apply(f: Future[Array[Byte]]): ByteChunk = Right(f.liftM[StreamT])
      def unapply(s: ByteChunk): Future[Array[Byte]] = ByteChunk.forceByteArray(s)
    }
  }

  implicit def chunkToFutureByteArray(implicit executor: ExecutionContext) = futureByteArrayToChunk.inverse

  /// String Bijections ///

  implicit val StringToByteArray = new Bijection[String, Array[Byte]] {
    def apply(s: String): Array[Byte] = s.getBytes("UTF-8")
    def unapply(t: Array[Byte]): String   = new String(t, "UTF-8")
  }

  implicit val ByteArrayToString = StringToByteArray.inverse

  implicit def stringToChunk(s: String): ByteChunk = Left(s.getBytes("UTF-8"))

  implicit def futureStringToChunk(implicit executor: ExecutionContext): Bijection[Future[String], ByteChunk] = {
    new Bijection[Future[String], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(executor)

      def apply(t: Future[String]): ByteChunk =
        Right(t.map(_.getBytes("UTF-8")).liftM[StreamT])

      def unapply(s: ByteChunk): Future[String] =
        ByteChunk.forceByteArray(s).map(new String(_, "UTF-8"))
    }
  }

  implicit def chunkToFutureString(implicit executor: ExecutionContext) = futureStringToChunk.inverse

  /// JValue Bijections ///

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

  implicit val ByteArrayToJValue = JValueToByteArray.inverse

  implicit def jvalueToChunk(jv: JValue): ByteChunk = Left(JValueToByteArray(jv))

  implicit def futureJValueToChunk(implicit executor: ExecutionContext): Bijection[Future[JValue], ByteChunk] = {
    new Bijection[Future[JValue], ByteChunk] {
      private implicit val M: Monad[Future] = new FutureMonad(executor)

      def apply(t: Future[JValue]) = {
        Right(t.map(JValueToByteArray(_)).liftM[StreamT])
      }

      // TODO: we could use the async parser here to avoid forcing
      def unapply(s: ByteChunk) =
        ByteChunk.forceByteArray(s) map { bytes =>
          JParser.parseFromByteBuffer(ByteBuffer.wrap(bytes)) | JUndefined
        }
    }
  }

  implicit def chunkToFutureJValue(implicit executor: ExecutionContext) = futureJValueToChunk.inverse
}

object DefaultBijections extends DefaultBijections
