package blueeyes.core.data

import scala.xml.NodeSeq
import scala.xml.XML
import blueeyes.concurrent.Future

trait BijectionsChunkXML {
  import java.io.{ByteArrayInputStream}
  implicit val XMLToChunk = new Bijection[NodeSeq, ByteChunk] {
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

trait BijectionsChunkFutureXML{
  import BijectionsChunkXML._
  implicit val FutureXMLToChunk = new Bijection[Future[NodeSeq], ByteChunk]{
    def apply(t: Future[NodeSeq]) = t.map(XMLToChunk(_)).value.getOrElse(new MemoryChunk(Array[Byte]()))

    def unapply(b: ByteChunk) = AggregatedByteChunk(b, None).map(ChunkToXML(_))
  }

  implicit val ChunkToFutureXML  = FutureXMLToChunk.inverse
}

object BijectionsChunkFutureXML extends BijectionsChunkFutureXML
