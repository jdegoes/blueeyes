package blueeyes.core.service

sealed trait ConnectionToken extends Product {
  def value = productPrefix

  override def toString = value

}

object ConnectionTokens {

  case object close extends ConnectionToken
  //case object `Keep-Alive` extends ConnectionToken //Depreciated in HTTP 1.1
  //case object `Persist` extends ConnectionToken  //Depreciated in HTTP 1.1
  sealed case class CustomConnectionToken(override val value: String) extends ConnectionToken {
    override def toString = value
  }

}
