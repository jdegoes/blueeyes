package blueeyes.health

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST.JNum

class ExportedStatisticSpec extends Specification with blueeyes.json.Implicits{
  "ExportedStatistic: gets lazy value" in{
    var value: Int = 0
    def lazyF: Int = value

    val statistic = new ExportedStatistic(lazyF)

    value = 2

    statistic.details must_== (2)
  }
  "ExportedStatistic: creates JValue" in{
    var value: Int = 0
    def lazyF: Int = value

    val statistic = new ExportedStatistic(lazyF)

    value = 1

    statistic.toJValue mustEqual (JNum(1))
  }
}