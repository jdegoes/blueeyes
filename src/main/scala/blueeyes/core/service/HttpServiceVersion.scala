package blueeyes.core.service

case class HttpServiceVersion(majorVersion: Int, minorVersion: Int, version: String)

trait HttpServiceVersionImplicits{
  implicit def stringToVersion(value: String) = {
    val major   = value.split("\\.").drop(0).headOption.map(_.toInt).getOrElse(0)

    val minor   = value.split("\\.").drop(1).headOption.map(_.toInt).getOrElse(0)

    val version = value.split("\\.").drop(2).headOption.getOrElse("")

    HttpServiceVersion(major, minor, version)
  }
}

object HttpServiceVersionImplicits extends HttpServiceVersionImplicits