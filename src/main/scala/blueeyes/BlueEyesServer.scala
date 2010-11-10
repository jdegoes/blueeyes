package blueeyes

import blueeyes.core.http._
import blueeyes.core.service._
import blueeyes.core.service.engines._
import blueeyes.util.{Future, FutureImplicits}

trait BlueEyesServer extends HttpServer[Array[Byte]] with HttpReflectiveServiceList[Array[Byte]] with NettyEngineArrayByte {

}