package blueeyes.util.logging

import RollPolicies._
import org.specs.Specification
import java.util.Date
import java.text.SimpleDateFormat

class NameFormatSpec extends Specification with NameFormat{
  private val baseName = "w3.log"
  "NameFormat: formats name with Never policy" in{
    checkName(Never, new SimpleDateFormat("yyyy"))
  }
  "NameFormat: formats name with Hourly policy" in{
    checkName(Hourly, new SimpleDateFormat("yyyyMMdd-HH"))
  }
  "NameFormat: formats name with Daily policy" in{
    checkName(Daily, new SimpleDateFormat("yyyyMMdd"))
  }
  "NameFormat: formats name with Weekly policy" in{
    checkName(Weekly(3), new SimpleDateFormat("yyyyMMdd"))
  }

  private def checkName(policy: Policy, format: SimpleDateFormat){
    val name = timedName(baseName, policy, 0)
    format.parse(name.substring(3, name.length - 4))
  }
}