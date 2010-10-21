package blueeyes.core.service
/*
  Encodings for AcceptEncoding
*/

sealed trait Encoding extends ProductPrefixUnmangler {
  def value: String = unmangledName 

  override def toString = value
}

object Encodings {
  case object compress extends Encoding
  case object gzip extends Encoding
  case object identity extends Encoding
  case object `x-compress` extends Encoding 
  case object `x-zip` extends Encoding
  case object `*` extends Encoding
  
  sealed case class CustomEncoding(override val value: String) extends Encoding
}
