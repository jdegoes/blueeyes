package blueeyes.core.service.engines

import blueeyes.core.data._
import blueeyes.core.http.{HttpHeaders, HttpRequest}

trait WebSocketUtils {
  def isWebSocketRequest(request: HttpRequest[ByteChunk]) = {
    val headers = request.headers
    headers.get(HttpHeaders.`Sec-WebSocket-Version`.name).map{_ =>
      headers.get(HttpHeaders.`Sec-WebSocket-Key`.name).map(_ => true).getOrElse{
        (headers.get(HttpHeaders.Connection.name), headers.get(HttpHeaders.Upgrade.name)) match {
          case (Some(connection), Some(upgrade)) => connection.equalsIgnoreCase("Upgrade") && upgrade.equalsIgnoreCase("WebSocket")
          case _ => false
        }
      }
    }.getOrElse(false)
  }
}