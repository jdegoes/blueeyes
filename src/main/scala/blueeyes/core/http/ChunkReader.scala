package blueeyes.core.http

import blueeyes.core.data.Bijection
import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST.JValue

import scala.xml.NodeSeq
import scala.xml.XML

trait ChunkReader{
  def hasNextChunk(): Boolean

  def nextChunk: Array[Byte]
}

class OneChunkReader(val chunk: Array[Byte]) extends ChunkReader{
  private var done = false
  def nextChunk = {
    done = true
    chunk
  }

  def hasNextChunk() = !done
}

trait BijectionsChunkReader{
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}
  implicit val StringToChunkReader = new Bijection[String, ChunkReader] {
    def apply(s: String): ChunkReader   = new OneChunkReader(s.getBytes("UTF-8"))
    def unapply(t: ChunkReader): String = new String(t.nextChunk, "UTF-8")
  }
  implicit val JValueToChunkReader      = new Bijection[JValue, ChunkReader]{
    def apply(t: JValue)         = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream))

      new OneChunkReader(stream.toByteArray())
    }
    def unapply(s: ChunkReader)  = JsonParser.parse(new InputStreamReader(new ByteArrayInputStream(s.nextChunk)))
  }
  implicit val ArrayByteToChunkReader = new Bijection[Array[Byte], ChunkReader] {
    def apply(t: Array[Byte]): ChunkReader    = new OneChunkReader(t)
    def unapply(s: ChunkReader): Array[Byte]  = s.nextChunk
  }

  implicit val XMLToChunkReader   = new Bijection[NodeSeq, ChunkReader] {
    def apply(s: NodeSeq)          = new OneChunkReader(s.toString.getBytes)
    def unapply(t: ChunkReader)    = XML.load(new ByteArrayInputStream(t.nextChunk))
  }

  implicit val ChunkReaderToString    = StringToChunkReader.inverse
  implicit val ChunkReaderToArrayByte = ArrayByteToChunkReader.inverse
  implicit val ChunkReaderToJValue    = JValueToChunkReader.inverse
  implicit val ChunkReaderToXML       = XMLToChunkReader.inverse
}

object BijectionsChunkReader extends BijectionsChunkReader