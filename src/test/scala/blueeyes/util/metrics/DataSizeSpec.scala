package blueeyes.util.metrics

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._
import org.scalacheck._

class DataSizeSpec extends Specification with ScalaCheck{
  private val sizes = Map[DataSizeUnit, Double](Bytes -> 1.0, KiloBytes -> 1024.0, MegaBytes -> 1048576.0, GigaBytes -> 1073741824.0, TeraBytes -> 1099511627776.0, PetaBytes -> 1125899906842624.0, ExaBytes -> 1152921504606846976.0, ZettaBytes -> 1180591620717411303424.0, YottaBytes -> 1.2089258196146292E24)

  val dataSizeUnitGen          = Gen.oneOf(Bytes, KiloBytes, MegaBytes, GigaBytes, TeraBytes, PetaBytes, ExaBytes, ZettaBytes, YottaBytes)
  implicit val arbDataSizeUnit = Arbitrary[DataSizeUnit](dataSizeUnitGen)
  implicit val arbDataSize     = Arbitrary[DataSize]{
    for {
      size <- Gen.choose(0, 1000)
      unit <- dataSizeUnitGen
    } yield DataSize(size, unit)
  }

  "DataSize" should{
    "convert different units" in{
      check { (dataSize: DataSize, unit: DataSizeUnit) =>
        dataSize.convert(unit) == DataSize(dataSize.size * (dataSize.unit.bytesInUnit / unit.bytesInUnit), unit)
      }
    }
  }
}