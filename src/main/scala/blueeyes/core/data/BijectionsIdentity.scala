package blueeyes.core.data

import blueeyes.json.JsonAST.JValue

import scala.xml.NodeSeq

trait BijectionsIdentity{
  implicit val JValueToJValue         = Bijection.identity[JValue]
  implicit val StringToString         = Bijection.identity[String]
  implicit val ArrayByteToArrayByte   = Bijection.identity[Array[Byte]]
  implicit val XMLToXML               = Bijection.identity[NodeSeq]
  implicit val ByteChunkToByteChunk   = Bijection.identity[ByteChunk]
}
object BijectionsIdentity extends BijectionsIdentity