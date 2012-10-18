package blueeyes.health

import org.specs2.mutable.Specification
import blueeyes.json.JNum

class ExportedStatisticSpec extends Specification {
  "ExportedStatistic: gets lazy value" in{
    var value: Int = 0
    def lazyF: JNum = JNum(value)

    val statistic = new ExportedStatistic(lazyF)

    value = 2

    statistic.details mustEqual (JNum(2))
  }
  "ExportedStatistic: creates JValue" in{
    var value: Int = 0
    def lazyF: JNum = JNum(value)

    val statistic = new ExportedStatistic(lazyF)

    value = 1

    statistic.toJValue mustEqual (JNum(1))
  }
}