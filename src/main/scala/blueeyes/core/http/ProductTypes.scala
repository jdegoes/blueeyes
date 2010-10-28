package blueeyes.core.http

/* For use in the Upgrade HTTP and X-Powered-By Headers */

sealed trait ProductType {

  def product: String
  def version: Option[String]
  def value = ((product.toList) ::: (version.toList)) .mkString("/")
  override def toString = value

}

object ProductTypes {

  def parseProductTypes(inString: String): Option[Array[ProductType]] = {
    def outProducts: Array[ProductType] = inString.trim.split(",").map(_.trim.split("/") match {
      case Array(x, y) => CustomProduct(x, y)
      case Array(x) => CustomProduct(x)
    })
    return Some(outProducts)
  }

  case class CustomProduct(product: String, version: Option[String]) extends ProductType
  object CustomProduct {
    def apply(product: String): ProductType = CustomProduct(product, None);
    def apply(product: String, version: String): ProductType = CustomProduct(product, Some(version));
  }

}
