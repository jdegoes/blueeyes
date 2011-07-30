package blueeyes.core.data

import org.specs.Specification
import blueeyes.concurrent.Future
import blueeyes.util.metrics.DataSize
import DataSize._

class AggregatedByteChunkSpec extends Specification{
  "AggregatedByteChunk" should {
    "aggregate full content when size is not specified" in{
      val chunk = new ByteMemoryChunk(Array[Byte]('1', '2'), () => Some(Future.sync(new ByteMemoryChunk(Array[Byte]('3', '4')))))
      AggregatedByteChunk(chunk, None).value.map(v => new String(v.data)) must eventually  (beSome("1234"))
    }
    "aggregate content up to the specified size" in{
      val chunk = new ByteMemoryChunk(Array[Byte]('1', '2'), () => Some(Future.sync(new ByteMemoryChunk(Array[Byte]('3', '4')))))
      AggregatedByteChunk(chunk, Some(2.bytes)).value.map(v => new String(v.data)) must eventually  (beSome("12"))
    }
  }
}