package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.JsonParser

import scala.xml.NodeSeq
import scala.xml.XML

trait BijectionsByteArray {
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream, PrintStream}
  import java.nio.ByteBuffer

  implicit val JValueToByteArray = new Bijection[JValue, Array[Byte]]{
    def unapply(arr: Array[Byte]) = {
      val bb = ByteBuffer.wrap(arr)
      val r = JsonParser.parseFromByteBuffer(bb)
      r.valueOr(e => throw e)
    }
    def apply(t: JValue)         = {
      val stream = new ByteArrayOutputStream()
      val printer = new PrintStream(stream, false, "UTF-8")
      printer.append(t.renderCompact)
      printer.close()
      stream.toByteArray()
    }
  }
  implicit val XMLToByteArray   = new Bijection[NodeSeq, Array[Byte]] {
    def apply(s: NodeSeq)       = s.toString.getBytes("UTF-8")
    def unapply(t: Array[Byte]) = XML.loadString(new String(t, "UTF-8"))
  }
  implicit val ByteArrayToString    = new Bijection[Array[Byte], String] {
    def apply(t: Array[Byte]): String   = new String(t, "UTF-8")
    def unapply(s: String): Array[Byte] = s.getBytes("UTF-8")
  }
  implicit val ByteArrayToJValue    = JValueToByteArray.inverse
  implicit val ByteArrayToXML       = XMLToByteArray.inverse
  implicit val StringToByteArray    = ByteArrayToString.inverse
}
object BijectionsByteArray extends BijectionsByteArray
