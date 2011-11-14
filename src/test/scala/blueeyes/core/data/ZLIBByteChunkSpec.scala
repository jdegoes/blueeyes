package blueeyes.core.data

import org.specs2.mutable.Specification
import blueeyes.concurrent.Future
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.zip.{Inflater, InflaterInputStream}

class ZLIBByteChunkSpec extends Specification{
  "GZICompressedByteChunk" should{
    "compress one chunk" in{
      testCompressed(new MemoryChunk("foo".getBytes, () => None), "foo")
    }
    "compress several chunks" in{
      testCompressed(new MemoryChunk("foo".getBytes, () => Some(Future.sync(new MemoryChunk("bar".getBytes, () => None)))), "foobar")
    }
  }

  private def testCompressed(chunk: ByteChunk, data: String) = {
    val compressed = ZLIBByteChunk(chunk)
    val future     = AggregatedByteChunk(compressed, None)

    future.isDelivered must eventually (be_==(true))

    new String(decopress(future.value.get)) mustEqual(data)
  }

  private def decopress(chunk: ByteChunk) = {
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