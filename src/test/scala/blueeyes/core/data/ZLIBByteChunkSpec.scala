package blueeyes.core.data

import akka.dispatch.Future
import blueeyes.bkka.AkkaDefaults
import blueeyes.concurrent.test.FutureMatchers

import org.specs2.mutable.Specification
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.zip.{Inflater, InflaterInputStream}

class ZLIBByteChunkSpec extends Specification with FutureMatchers with AkkaDefaults {
  "GZICompressedByteChunk" should{
    "compress one chunk" in{
      testCompressed(new MemoryChunk("foo".getBytes, () => None), "foo")
    }
    "compress several chunks" in{
      testCompressed(new MemoryChunk("foo".getBytes, () => Some(Future(new MemoryChunk("bar".getBytes, () => None)))), "foobar")
    }
  }

  private def testCompressed(chunk: ByteChunk, data: String) = {
    val compressed = ZLIBByteChunk(chunk)
    val future     = AggregatedByteChunk(compressed, None)

    future.map(v => new String(decompress(v))) must whenDelivered { be_==(data) }
  }

  private def decompress(chunk: ByteChunk) = {
    val in  = new InflaterInputStream(new ByteArrayInputStream(chunk.data), new Inflater())
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](1024)
    var len = in.read(buf)

    while (len > 0)
    do{
      out.write(buf, 0, len)
      len = in.read(buf)
    } while (len > 0)

    in.close()
    out.close()

    out.toByteArray
  }

}
