package blueeyes.core.http

sealed trait HttpMethod extends Product {
  def value = productPrefix
  
  override def toString = value
}

object HttpMethods {
  
  /* Perhaps needs some regex */
  def parseHttpMethods(inString: String): Array[HttpMethod] = {
    def outMethods: Array[HttpMethod] = inString.trim.toLowerCase.split(",").map(_.trim match {
      case "get"      => GET     
      case "put"      => PUT
      case "post"     => POST
      case "patch"    => PATCH
      case "delete"   => DELETE
      case "options"  => OPTIONS
      case "head"     => HEAD
      case "connect"  => CONNECT
      case "trace"    => TRACE
      case x          => CUSTOM(x)    // Perhaps shouldn't return custom?
    })
    return outMethods
  }
  

  case object GET extends HttpMethod

  case object PUT extends HttpMethod

  case object POST extends HttpMethod

  case object DELETE extends HttpMethod

  case object PATCH extends HttpMethod

  case object OPTIONS extends HttpMethod

  case object HEAD extends HttpMethod

  case object CONNECT extends HttpMethod

  case object TRACE extends HttpMethod

  case class CUSTOM(method: String) extends HttpMethod {
    override def toString = method
  }
}
