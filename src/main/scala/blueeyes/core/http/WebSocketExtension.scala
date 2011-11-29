package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler

sealed trait WebSocketExtension extends ProductPrefixUnmangler {
  def value: String = unmangledName

  override def toString = value
}

object WebSocketExtension{
  def parseExtensions(inString: String) = inString.split(",").map(parseExtension(_)).toList

  private def parseExtension(inString: String) = inString match {
    case "deflate-frame"  => `deflate-frame`
    case "deflate-stream" => `deflate-stream`
    case _ => CustomWebSocketExtension(inString)
  }

  case object `deflate-frame` extends WebSocketExtension
  case object `deflate-stream` extends WebSocketExtension

  sealed case class CustomWebSocketExtension(override val value: String) extends WebSocketExtension
}
