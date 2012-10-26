package blueeyes.core.data

import blueeyes.bkka._
import blueeyes.json._
import blueeyes.concurrent.test.FutureMatchers

import akka.dispatch.Await
import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import akka.util.Duration

import java.nio.ByteBuffer

import scalaz._

import org.specs2.mutable.Specification

class BijectionsChunkJsonSpec extends Specification with AkkaDefaults with FutureMatchers {
  import DefaultBijections._

  implicit val M: Monad[Future] = new FutureMonad(implicitly[ExecutionContext])
  val bijection = futureJValueToChunk(implicitly[ExecutionContext])

  "BijectionsChunkJson" should{
    "parse JSON split across chunks" in {
      val b1 = ByteBuffer.wrap("""{"foo":""".getBytes("UTF-8"))
      val b2 = ByteBuffer.wrap(""""bar"}""".getBytes("UTF-8"))

      val stream = Right(b1 :: b2 :: StreamT.empty[Future, ByteBuffer]) 
      bijection.unapply(stream) must whenDelivered {
        be_==(JObject(JField("foo", JString("bar")) :: Nil))
      }
    }

    "throw error when JSON is incomplete" in{
      val b1 = ByteBuffer.wrap("""{"foo":""".getBytes("UTF-8"))
      val b2 = ByteBuffer.wrap(""""bar"}""".getBytes("UTF-8"))

      val stream = Right(b1 :: b2 :: StreamT.empty[Future, ByteBuffer]) 
      Await.result(bijection.unapply(stream), Duration(500, "milliseconds")) must throwA[Exception]
    }
  }
}
