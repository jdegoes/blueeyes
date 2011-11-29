package blueeyes.core.http

import util.parsing.combinator.RegexParsers

case class WebSocketProtocol(protocol: String)

object WebSocketProtocol extends RegexParsers {
  def parseProtocol(inString: String) = inString.split(",").map(_.trim).map(WebSocketProtocol(_)).toList
}