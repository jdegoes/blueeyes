package blueeyes

import core.data.{BijectionsString, BijectionsByteArray}
import core.http._
import core.service.HttpResponseHandlerCombinators
import util.FutureImplicits

trait BlueEyesResponseHandlerBuilderBase[T] extends HttpResponseHandlerCombinators with
    FutureImplicits with
    HttpHeaderImplicits with
    HttpStatusImplicits with
    HttpStatusCodeImplicits with
    HttpDateImplicits with
    HttpNumberImplicits{
}

trait BlueEyesResponseHandlerBuilder extends BlueEyesResponseHandlerBuilderBase[Array[Byte]] with BijectionsByteArray

trait BlueEyesResponseHandlerBuilderString extends BlueEyesResponseHandlerBuilderBase[String] with BijectionsString
