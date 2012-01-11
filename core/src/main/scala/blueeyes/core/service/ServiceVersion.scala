package blueeyes.core.service

case class ServiceVersion(majorVersion: Int, minorVersion: Int, version: String)

trait ServiceVersionImplicits {
  implicit def fromString(value: String) = {
    val major   = value.split("\\.").drop(0).headOption.map(_.toInt).getOrElse(0)
    val minor   = value.split("\\.").drop(1).headOption.map(_.toInt).getOrElse(0)
    val version = value.split("\\.").drop(2).headOption.getOrElse("")

    ServiceVersion(major, minor, version)
  }
}

object ServiceVersion extends ServiceVersionImplicits
