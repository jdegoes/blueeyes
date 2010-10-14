package blueeyes.core.service

sealed trait HttpMethod extends Product {
  def value = productPrefix
  
  override def toString = value
}

object HttpMethods {
  case object GET extends HttpMethod

  case object PUT extends HttpMethod

  case object POST extends HttpMethod

  case object DELETE extends HttpMethod

  case object OPTIONS extends HttpMethod

  case object HEAD extends HttpMethod

  case object CONNECT extends HttpMethod

  case object TRACE extends HttpMethod

  case class CUSTOM(method: String) extends HttpMethod {
    override def toString = method
  }
}