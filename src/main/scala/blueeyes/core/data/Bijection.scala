package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonAST.JValue

import scala.xml.NodeSeq
import scala.xml.XML
import blueeyes.concurrent.FutureDeliveryStrategySequential

trait Bijection[T, S] { self =>
  def isDefinedAt(t: T): Boolean = {
    try {
      apply(t)
      
      true
    }
    catch {
      case _ => false
    }
  }
  
  def apply(t: T): S
  
  def unapply(s: S): T
  
  def inverse: Bijection[S, T] = new Bijection[S, T] {
    def apply(s: S): T   = self.unapply(s)
    def unapply(t: T): S = self.apply(t)
  }
  
  def compose[R](that: Bijection[R, T]): Bijection[R, S] = new Bijection[R, S] {
    def apply(r: R): S = self.apply(that.apply(r))
    
    def unapply(s: S): R = that.unapply(self.unapply(s))
  }
  
  def andThen[R](that: Bijection[S, R]): Bijection[T, R] = that.compose(self)
}

object Bijection {
  def identity[T]: Bijection[T, T] = new Bijection[T, T] {
    def apply(t: T): T = t
    
    def unapply(t: T): T = t
  }
}

trait BijectionsIdentity{
  implicit val JValueToJValue         = Bijection.identity[JValue]
  implicit val StringToString         = Bijection.identity[String]
  implicit val ArrayByteToArrayByte   = Bijection.identity[Array[Byte]]
  implicit val XMLToXML               = Bijection.identity[NodeSeq]
}
object BijectionsIdentity extends BijectionsIdentity

trait BijectionsByteArray {
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  implicit val JValueToByteArray    = new Bijection[JValue, Array[Byte]]{
    def unapply(s: Array[Byte])  = JsonParser.parse(new InputStreamReader(new ByteArrayInputStream(s)))
    def apply(t: JValue)         = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream))

      stream.toByteArray()
    }
  }
  implicit val XMLToByteArray   = new Bijection[NodeSeq, Array[Byte]] {
    def apply(s: NodeSeq)       = s.toString.getBytes
    def unapply(t: Array[Byte]) = XML.load(new ByteArrayInputStream(t))
  }
  implicit val ByteArrayToString    = new Bijection[Array[Byte], String] {
    def apply(t: Array[Byte]): String   = new String(t)
    def unapply(s: String): Array[Byte] = s.getBytes
  }
  implicit val ByteArrayToJValue    = JValueToByteArray.inverse
  implicit val ByteArrayToXML       = XMLToByteArray.inverse
  implicit val StringToByteArray    = ByteArrayToString.inverse
}
object BijectionsByteArray extends BijectionsByteArray

trait BijectionsChunkReaderJson{
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  implicit val JValueToChunkReader      = new Bijection[JValue, Chunk]{
    def apply(t: JValue)         = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream))

      new MemoryChunk(stream.toByteArray())
    }
    def unapply(s: Chunk)  = JsonParser.parse(new InputStreamReader(new ByteArrayInputStream(s.data)))
  }

  implicit val ChunkReaderToJValue    = JValueToChunkReader.inverse
}
object BijectionsChunkReaderJson extends BijectionsChunkReaderJson

trait BijectionsChunkReaderString {
  implicit val StringToChunkReader = new Bijection[String, Chunk] {
    def apply(s: String): Chunk   = new MemoryChunk(s.getBytes("UTF-8"))
    def unapply(t: Chunk): String = new String(t.data, "UTF-8")
  }

  implicit val ChunkReaderToString    = StringToChunkReader.inverse
}
object BijectionsChunkReaderString extends BijectionsChunkReaderString

trait BijectionsChunkReaderByteArray {
  implicit val ArrayByteToChunkReader = new Bijection[Array[Byte], Chunk] {
    def apply(t: Array[Byte]): Chunk    = new MemoryChunk(t)
    def unapply(s: Chunk): Array[Byte]  = s.data
  }

  implicit val ChunkReaderToArrayByte = ArrayByteToChunkReader.inverse
}
object BijectionsChunkReaderByteArray extends BijectionsChunkReaderByteArray

trait BijectionsXML {
  import java.io.{ByteArrayInputStream}
  implicit val XMLToChunkReader   = new Bijection[NodeSeq, Chunk] {
    def apply(s: NodeSeq)          = new MemoryChunk(s.toString.getBytes)
    def unapply(t: Chunk)    = XML.load(new ByteArrayInputStream(t.data))
  }

  implicit val ChunkReaderToXML = XMLToChunkReader.inverse
}
object BijectionsXML extends BijectionsByteArray
