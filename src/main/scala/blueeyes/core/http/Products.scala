package blueeyes.core.http

/* For use in the Upgrade HTTP and X-Powered-By Headers */

sealed trait Product {

  def product: String
  def version: Option[String]
  def value = ((product.toList) ::: (version.toList)) .mkString("/")
  override def toString = value

}

object Products {

  def parseProducts(inString: String): Option[Array[Product]] = {
    def outProducts: Array[Product] = inString.trim.split(",").map(_.trim.split("/") match {
      case Array(x, y) => CustomProduct(x, y)
      case Array(x) => CustomProduct(x)
    })
    return Some(outProducts)
  }

  case class CustomProduct(product: String, version: Option[String]) extends Product
  object CustomProduct {
    def apply(product: String): Product = CustomProduct(product, None);
    def apply(product: String, version: String): Product = CustomProduct(product, Some(version));
  }

}
