package blueeyes.core.data

import akka.dispatch.Future

import blueeyes.bkka._
import blueeyes.concurrent.test.FutureMatchers

import org.specs2.mutable.Specification

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.nio.ByteBuffer
import java.util.zip.{Inflater, InflaterInputStream}

import scalaz._
import scalaz.syntax.monad._

class ZLIBChunkCompressionSpec extends Specification with FutureMatchers with TestAkkaDefaults {
  "GZICompressedByteChunk" should{
    "compress one chunk" in{
      testCompressed(Left(ByteBuffer.wrap("foo".getBytes)), "foo")
    }

    "compress several chunks" in{
      testCompressed(Right(ByteBuffer.wrap("foo".getBytes) :: Future(ByteBuffer.wrap("bar".getBytes)).liftM[StreamT]), "foobar")
    }
  }

  private def testCompressed(chunk: ByteChunk, data: String) = {
    val Right(compressed) = ChunkCompression.zlib(None).compress(chunk)

    compressed.head.map(v => new String(decompress(v.array))) must whenDelivered { 
      be_==(data) 
    }
  }

  private def decompress(data: Array[Byte]) = {
    val in  = new InflaterInputStream(new ByteArrayInputStream(data), new Inflater())
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
