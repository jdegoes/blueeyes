package blueeyes.core.data

import org.specs2.mutable.Specification
import akka.dispatch.Future
import blueeyes.bkka.AkkaDefaults
import blueeyes.util.metrics.DataSize
import blueeyes.concurrent.test.FutureMatchers
import DataSize._

class AggregatedByteChunkSpec extends Specification with AkkaDefaults with FutureMatchers {
  "AggregatedByteChunk" should {
    "aggregate full content when size is not specified" in{
      val chunk = Chunk(Array[Byte]('1', '2'), Some(Future(Chunk(Array[Byte]('3', '4')))))
      AggregatedByteChunk(chunk, None).map(v => new String(v.data)) must whenDelivered (be_==("1234"))
    }
    "aggregate content up to the specified size" in{
      val chunk = Chunk(Array[Byte]('1', '2'), Some(Future(Chunk(Array[Byte]('3', '4')))))
      AggregatedByteChunk(chunk, Some(2.bytes)).map(v => new String(v.data)) must whenDelivered (be_==("12"))
    }
  }
}
