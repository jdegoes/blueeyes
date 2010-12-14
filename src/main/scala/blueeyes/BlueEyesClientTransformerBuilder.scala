package blueeyes

import core.data.{BijectionsString, BijectionsByteArray}
import core.http._
import core.service.{HttpClientTransformerImplicits, HttpClientTransformerCombinators}
import util.FutureImplicits

trait BlueEyesClientTransformerBuilderBase[T] extends HttpClientTransformerCombinators with
    FutureImplicits with
    HttpHeaderImplicits with
    HttpStatusImplicits with
    HttpStatusCodeImplicits with
    HttpDateImplicits with
    HttpNumberImplicits with
    HttpClientTransformerImplicits{
}

trait BlueEyesClientTransformerBuilder extends BlueEyesClientTransformerBuilderBase[Array[Byte]] with BijectionsByteArray

trait BlueEyesClientTransformerBuilderString extends BlueEyesClientTransformerBuilderBase[String] with BijectionsString
