package blueeyes

import concurrent.{FutureDeliveryStrategySequential, FutureImplicits}
import core.data.{BijectionsString, BijectionsByteArray}
import core.http._
import core.service.{HttpClientTransformerImplicits}
trait BlueEyesClientTransformerBuilderBase[T] extends FutureImplicits with
    HttpHeaderImplicits with
    HttpStatusImplicits with
    HttpStatusCodeImplicits with
    HttpDateImplicits with
    HttpNumberImplicits with
    HttpClientTransformerImplicits with
    FutureDeliveryStrategySequential{
}

trait BlueEyesClientTransformerBuilder extends BlueEyesClientTransformerBuilderBase[Array[Byte]] with BijectionsByteArray

trait BlueEyesClientTransformerBuilderString extends BlueEyesClientTransformerBuilderBase[String] with BijectionsString
