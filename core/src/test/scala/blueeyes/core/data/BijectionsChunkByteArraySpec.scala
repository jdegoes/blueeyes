package blueeyes.core.data

import blueeyes.bkka._
import blueeyes.akka_testing.FutureMatchers

import akka.dispatch.Future
import akka.dispatch.ExecutionContext

import java.nio.ByteBuffer

import org.specs2.mutable.Specification

import scalaz._

class BijectionsChunkByteArraySpec extends Specification with TestAkkaDefaults with FutureMatchers {
  import DefaultBijections._

  val bijection = futureByteArrayToChunk(defaultFutureDispatch)

  "BijectionsChunkByteArray" should {
    "Convert a stream of chunks to a single byte array" in {
      val bytes1 = Array[Byte](0x01, 0x02, 0x03)
      val bytes2 = Array[Byte](0x04, 0x05, 0x06)
      val expected = Array[Byte](0x01, 0x02, 0x03, 0x04, 0x05, 0x06)

      val result = bijection.unapply(Right(bytes1 :: bytes2 :: StreamT.empty[Future, Array[Byte]]))

      result must whenDelivered {
        be_==(expected)
      }
    }
  }
}
