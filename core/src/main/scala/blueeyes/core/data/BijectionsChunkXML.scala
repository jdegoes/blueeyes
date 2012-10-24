package blueeyes.core.data

import scala.xml.NodeSeq
import scala.xml.XML
import akka.dispatch.Future
import akka.dispatch.Await
import akka.util.Timeout

trait BijectionsChunkXML {
  import java.io.{ByteArrayInputStream}
  implicit val XMLToChunk = new Bijection[NodeSeq, Chunk[Array[Byte]]] {
    def apply(s: NodeSeq)    = Chunk(s.toString.getBytes)
    def unapply(t: Chunk[Array[Byte]]) = try{
      XML.load(new ByteArrayInputStream(t.data))
    } catch {
      case e: org.xml.sax.SAXParseException => sys.error("Data is too big, use big data handler.")
    }
  }

  implicit val ChunkToXML = XMLToChunk.inverse
}
object BijectionsChunkXML extends BijectionsChunkXML

trait BijectionsChunkFutureXML{
  import BijectionsChunkXML._
  implicit def futureXMLToChunk(implicit timeout: Timeout = Timeout.zero) = new Bijection[Future[NodeSeq], Chunk[Array[Byte]]]{
    def apply(t: Future[NodeSeq]) = Await.result(t.map(XMLToChunk(_)), timeout.duration)

    def unapply(b: Chunk[Array[Byte]]) = AggregatedByteChunk(b, None).map(ChunkToXML(_))
  }

  implicit def chunkToFutureXML(implicit timeout: Timeout = Timeout.zero) = futureXMLToChunk(timeout).inverse
}

object BijectionsChunkFutureXML extends BijectionsChunkFutureXML
