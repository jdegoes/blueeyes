package blueeyes
package core
package service
package test

import scala.util.DynamicVariable

trait MockKeyBase {

  def name: String

  lazy val box: DynamicVariable[Option[Boolean]] =
    new DynamicVariable(sys.props.get(name).map(_.toBoolean))

  def value: Boolean

}

object GlobalKey extends MockKeyBase {

  val name = "blueeyes.mock"

  def value = box.value.getOrElse(false)

  def value_=(newValue: Option[Boolean]) {
    if(sys.props.get(name).isDefined)
      // Don't override a user specified setting
      ()
    else
      box.value = newValue
  }
}

case class MockKey(val name: String, val default: Boolean) extends MockKeyBase {

  val value: Boolean =
    box.value.getOrElse(GlobalKey.box.value.getOrElse(default))

}
