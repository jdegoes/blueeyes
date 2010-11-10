package blueeyes.core.service.engines

import blueeyes.core.service._

trait NettyEngineString extends HttpServerEngine[String] { self: HttpRequestHandler[String] =>
  
}

trait NettyEngineArrayByte extends HttpServerEngine[Array[Byte]] { self: HttpRequestHandler[Array[Byte]] =>
  
}