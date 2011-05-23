package blueeyes.util.metrics

sealed trait DataSizeUnit{
  def abbreviation: String

  def bytesInUnit: Double
}
case object Bytes extends DataSizeUnit {
  val abbreviation = "bytes"
  val bytesInUnit = 1.0
}
case object KiloBytes extends DataSizeUnit{
  val abbreviation = "kB"
  val bytesInUnit = 1024.0
}
case object MegaBytes extends DataSizeUnit{
  val abbreviation = "MB"
  val bytesInUnit = 1048576.0
}
case object GigaBytes extends DataSizeUnit{
  val abbreviation = "GB"
  val bytesInUnit = 1073741824.0
}
case object TeraBytes extends DataSizeUnit{
  val abbreviation = "TB"
  val bytesInUnit = 1099511627776.0
}
case object PetaBytes extends DataSizeUnit{
  val abbreviation = "PB"
  val bytesInUnit = 1125899906842624.0
}
case object ExaBytes extends DataSizeUnit{
  val abbreviation = "EB"
  val bytesInUnit = 1152921504606846976.0
}
case object ZettaBytes extends DataSizeUnit{
  val abbreviation = "ZB"
  val bytesInUnit = 1180591620717411303424.0
}
case object YottaBytes extends DataSizeUnit{
  val abbreviation = "YB"
  val bytesInUnit = 1.2089258196146292E24
}
