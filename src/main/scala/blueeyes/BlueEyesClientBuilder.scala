package blueeyes

import core.data.{BijectionsString, BijectionsByteArray}
import core.http._
import core.service.HttpResponseHandlerCombinators
import util.FutureImplicits

trait BlueEyesClientBuilderBase[T] extends HttpResponseHandlerCombinators with
    FutureImplicits with
    HttpHeaderImplicits with
    HttpStatusImplicits with
    HttpStatusCodeImplicits with
    HttpDateImplicits with
    HttpNumberImplicits{
}

trait BlueEyesClientBuilder extends BlueEyesClientBuilderBase[Array[Byte]] with BijectionsByteArray

trait BlueEyesClientBuilderString extends BlueEyesClientBuilderBase[String] with BijectionsString
