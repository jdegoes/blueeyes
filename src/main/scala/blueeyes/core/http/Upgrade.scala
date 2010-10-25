package blueeyes.core.http

/* For use in the Upgrade HTTP Header */

sealed trait UpgradeProduct {

  def product: String
  def version: Option[String]
  def value = (product :: version :: Nil).mkString("/")
  override def toString = value

}

object UpgradeProducts {

  def parseUpgradeProducts(inString: String): Array[UpgradeProduct] = {
    def outProducts: Array[UpgradeProduct] = inString.trim.split(",").map(_.trim.split("/") match {
      case Array(x, y) => CustomUpgradeProduct(x, y)
      case Array(x) => CustomUpgradeProduct(x)
    })
    return outProducts 
  }

  case class CustomUpgradeProduct(product: String, version: Option[String]) extends UpgradeProduct
  object CustomUpgradeProduct {
    def apply(product: String): UpgradeProduct = CustomUpgradeProduct(product, None);
    def apply(product: String, version: String): UpgradeProduct = CustomUpgradeProduct(product, Some(version));
  }

}
