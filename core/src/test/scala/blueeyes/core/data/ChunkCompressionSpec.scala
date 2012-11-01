package blueeyes.core.data

import org.specs2.mutable.Specification

import java.util.zip.GZIPInputStream
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import akka.dispatch.Future
import blueeyes.bkka._
import blueeyes.akka_testing.FutureMatchers

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.nio.ByteBuffer
import java.util.zip.{Inflater, InflaterInputStream}

import scalaz._
import scalaz.syntax.monad._

trait ChunkCompressionSpec extends Specification with TestAkkaDefaults with FutureMatchers {
  def testCompression(compression: ChunkCompression, chunk: ByteChunk, data: String) = {
    compression.compress(chunk) match {
      case Right(compressed) =>
        compressed.head.map(buf => new String(decompress(buf.array))) must whenDelivered {
          be_==(data)
        }

      case Left(compressed) =>
        new String(decompress(compressed.array)) must_== data
    }
  }

  def decompress(data: Array[Byte]): Array[Byte]
}

class GZIPChunkCompressionSpec extends ChunkCompressionSpec {
  "GZIPChunkCompression" should {
    "compress one chunk" in{
      testCompression(ChunkCompression.gzip, ByteChunk("foo".getBytes), "foo")
    }
    "compress several chunks" in{
      testCompression(ChunkCompression.gzip, Right(ByteBuffer.wrap("foo".getBytes) :: Future(ByteBuffer.wrap("bar".getBytes)).liftM[StreamT]), "foobar")
    }
  }

  def decompress(data: Array[Byte]) = {
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

class ZLIBChunkCompressionSpec extends ChunkCompressionSpec {
  "GZICompressedByteChunk" should{
    "compress one chunk" in{
      testCompression(ChunkCompression.zlib(None), Left(ByteBuffer.wrap("foo".getBytes)), "foo")
    }

    "compress several chunks" in{
      testCompression(ChunkCompression.zlib(None), Right(ByteBuffer.wrap("foo".getBytes) :: Future(ByteBuffer.wrap("bar".getBytes)).liftM[StreamT]), "foobar")
    }
  }

  def decompress(data: Array[Byte]) = {
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
