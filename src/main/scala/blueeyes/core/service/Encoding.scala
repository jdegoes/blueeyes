package blueeyes.core.service
/*
  Encodings for AcceptEncoding
*/

sealed trait Encoding {

  def encodingName: String
  def value: String = encodingName

}

object Encodings {

  trait GenericEncoding extends Product with Encoding {
    def encodingName = productPrefix
  }

  case object compress extends GenericEncoding
  case object gzip extends GenericEncoding
  case object identity extends GenericEncoding
  case object `x-compress` extends GenericEncoding 
  case object `x-zip` extends GenericEncoding

}
