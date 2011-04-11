package blueeyes

import concurrent.{FutureDeliveryStrategySequential, FutureImplicits}
import core.data.{BijectionsString, BijectionsByteArray}
import core.http._
trait BlueEyesClientBuilderBase[T] extends FutureImplicits with
    HttpHeaderImplicits with
    HttpStatusImplicits with
    HttpStatusCodeImplicits with
    HttpDateImplicits with
    HttpNumberImplicits with
    FutureDeliveryStrategySequential{
}

trait BlueEyesClientBuilder extends BlueEyesClientBuilderBase[Array[Byte]] with BijectionsByteArray

trait BlueEyesClientBuilderString extends BlueEyesClientBuilderBase[String] with BijectionsString
