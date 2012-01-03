package blueeyes.core.service.engines

import blueeyes.core.data._
import blueeyes.core.http.HttpHeaders

trait WebSocketUtils {
  def isWebSocketRequest(headers: HttpHeaders) = {
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