package blueeyes.core.data

import org.specs.Specification
import java.util.zip.GZIPInputStream
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import blueeyes.concurrent.Future

class GZIPByteChunkSpec extends Specification{
  "GZICompressedByteChunk" should{
    "compress one chunk" in{
      testCompressed(new MemoryChunk("foo".getBytes, () => None), "foo")
    }
    "compress several chunks" in{
      testCompressed(new MemoryChunk("foo".getBytes, () => Some(Future.sync(new MemoryChunk("bar".getBytes, () => None)))), "foobar")
    }
  }

  private def testCompressed(chunk: ByteChunk, data: String) = {
    val compressed = GZIPByteChunk(chunk)
    val future     = AggregatedByteChunk(compressed, None)

    future.isDelivered must eventually (be(true))

    new String(decopress(future.value.get)) mustEqual(data)
  }

  private def decopress(chunk: ByteChunk) = {
    val in  = new GZIPInputStream(new ByteArrayInputStream(chunk.data))
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