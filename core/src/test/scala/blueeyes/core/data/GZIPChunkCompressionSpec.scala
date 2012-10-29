package blueeyes.core.data

import org.specs2.mutable.Specification

import java.util.zip.GZIPInputStream
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import akka.dispatch.Future
import blueeyes.bkka._
import blueeyes.akka_testing.FutureMatchers

import java.nio.ByteBuffer

import scalaz._
import scalaz.syntax.monad._

class GZIPChunkCompressionSpec extends Specification with FutureMatchers with TestAkkaDefaults {
  "GZICompressedByteChunk" should{
    "compress one chunk" in{
      testCompressed(ByteChunk("foo".getBytes), "foo")
    }
    "compress several chunks" in{
      testCompressed(Right(ByteBuffer.wrap("foo".getBytes) :: Future(ByteBuffer.wrap("bar".getBytes)).liftM[StreamT]), "foobar")
    }
  }

  private def testCompressed(chunk: ByteChunk, data: String) = {
    val Right(compressed) = ChunkCompression.gzip.compress(chunk)
    compressed.head.map(buf => new String(decompress(buf.array))) must whenDelivered {
      be_==(data)
    }
  }

  private def decompress(data: Array[Byte]) = {
    val in  = new GZIPInputStream(new ByteArrayInputStream(data))
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](1024)

    var len = in.read(buf)
    if (len > 0) {
      do {
        out.write(buf, 0, len)
        len = in.read(buf)
      } while (len > 0)
    }

    in.close()
    out.close()

    out.toByteArray
  }
}
