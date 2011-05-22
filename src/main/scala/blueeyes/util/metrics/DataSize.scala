package blueeyes.util.metrics

case class DataSize(size: Double, unit: DataSizeUnit){
  def bytes = convert(Bytes)

  def kilobytes = convert(KiloBytes)
  def kb = kilobytes

  def megabytes = convert(MegaBytes)
  def mb = megabytes

  def gigabytes = convert(GigaBytes)
  def gb = gigabytes

  def terabytes = convert(TeraBytes)
  def tb = terabytes

  def petabytes = convert(PetaBytes)
  def pt = petabytes

  def exabytes = convert(ExaBytes)
  def eb = exabytes

  def zettabytes = convert(ZettaBytes)
  def zb = zettabytes

  def yottabytes = convert(YottaBytes)
  def yb = yottabytes

  def convert(u: DataSizeUnit) = DataSize((size * ratio(unit, u)), u)

  override def toString = "%2.2f%s".format(size, unit.abbreviation)

  private def ratio(unit: DataSizeUnit, u: DataSizeUnit): Double = unit.bytesInUnit / u.bytesInUnit
}

object DataSize {
  // So user can write: 23.bytes, 92.gigabytes, etc.
  class ToDataSize[N](numeric: N)(implicit num: Numeric[N]) {
    private val value = num.toLong(numeric)
    def bytes       = DataSize(value, Bytes)
    def kilobytes   = DataSize(value, KiloBytes)
    def megabytes   = DataSize(value, MegaBytes)
    def gigabytes   = DataSize(value, GigaBytes)
    def terabytes   = DataSize(value, TeraBytes)
    def petabytes   = DataSize(value, PetaBytes)
    def exabytes    = DataSize(value, ExaBytes)
    def zettabytes  = DataSize(value, ZettaBytes)
    def yottabytes  = DataSize(value, YottaBytes)
  }

  implicit def toDataSize[N](numeric: N)(implicit num: Numeric[N]): ToDataSize[N] = new ToDataSize(numeric)
}