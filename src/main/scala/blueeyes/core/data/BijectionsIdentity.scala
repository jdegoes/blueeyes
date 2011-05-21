package blueeyes.core.data

import blueeyes.json.JsonAST.JValue

import scala.xml.NodeSeq

trait BijectionsIdentity{
  implicit def identity[T]: Bijection[T, T] = new Bijection[T, T] {
    def apply(t: T): T = t

    def unapply(t: T): T = t
  }

  implicit val JValueToJValue         = Bijection.identity[JValue]
  implicit val StringToString         = Bijection.identity[String]
  implicit val ArrayByteToArrayByte   = Bijection.identity[Array[Byte]]
  implicit val XMLToXML               = Bijection.identity[NodeSeq]
  implicit val ByteChunkToByteChunk   = Bijection.identity[ByteChunk]
}
object BijectionsIdentity extends BijectionsIdentity