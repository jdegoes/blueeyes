package blueeyes.core.data

import akka.dispatch.Future
import blueeyes.bkka._

import java.nio.ByteBuffer

import blueeyes.akka_testing.FutureMatchers
import org.specs2.mutable.Specification


import scalaz._

class ByteChunkSpec extends Specification with TestAkkaDefaults with FutureMatchers {
  "aggregation" should {
    "aggregate full content when size is not specified" in{
      val stream = Array[Byte]('1', '2') :: 
                   Array[Byte]('3', '4') :: 
                   StreamT.empty[Future, Array[Byte]]

      ByteChunk.aggregate(Right(stream), 1024).right.get.head.map {
        _.toList
      } must whenDelivered {
        be_==(List[Byte]('1','2','3','4'))
      }
    }

    "aggregate content up to the specified size" in{
      val stream = Array[Byte]('1', '2') :: 
                   Array[Byte]('3', '4') :: 
                   StreamT.empty[Future, Array[Byte]]

      ByteChunk.aggregate(Right(stream), 2).right.get.head.map {
        _.toList
      } must whenDelivered {
        be_==(List[Byte]('1','2'))
      }
    }
  }
}
