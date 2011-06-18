package blueeyes.core.data

import scala.xml.NodeSeq
import scala.xml.XML

trait BijectionsChunkXML {
  import java.io.{ByteArrayInputStream}
  implicit val XMLToChunk   = new Bijection[NodeSeq, ByteChunk] {
    def apply(s: NodeSeq)    = new MemoryChunk(s.toString.getBytes)
    def unapply(t: ByteChunk)    = try{
      XML.load(new ByteArrayInputStream(t.data))
    }
    catch {
      case e: org.xml.sax.SAXParseException => sys.error("Data is too big, use big data handler.")
    }
  }

  implicit val ChunkToXML = XMLToChunk.inverse
}
object BijectionsChunkXML extends BijectionsChunkXML
