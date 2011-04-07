package blueeyes.health

import org.specs.Specification
import blueeyes.json.JsonAST.JInt

class ExportedStatisticSpec extends Specification with blueeyes.json.Implicits{
  "ExportedStatistic: gets lazy value" in{
    var value: Int = 0
    def lazyF: Int = value

    val statistic = new ExportedStatistic(lazyF)

    value = 2

    statistic.details must be (2)
  }
  "ExportedStatistic: creates JValue" in{
    var value: Int = 0
    def lazyF: Int = value

    val statistic = new ExportedStatistic(lazyF)

    value = 1

    statistic.toJValue mustEqual (JInt(1))
  }
}