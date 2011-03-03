package blueeyes.parsers

import org.scalacheck._
import Gen._

object W3ExtendedLogGen{

  def directives          = listOfN(7, oneOf(versionDirective, softwareDirective, fieldsDirective, startDateDirective, endDateDirective, dateDirective, remarkDirective)) map {v => v.mkString("\n")}

  def versionDirective    = listOfN(2, numChar) map {v => "#Version: " + v.mkString(".")}

  def softwareDirective   = identifier map {v => "#Software: " + v}

  def fieldsDirective     = listOfN(20, fieldDirective) map{v => "#Fields: " + v.mkString(" ")}

  def startDateDirective  = dateTime map {v => "#Start-Date: " + v }

  def endDateDirective    = dateTime map {v => "#End-Date: " + v }

  def dateDirective       = dateTime map {v => "#Date: " + v }

  def remarkDirective     = identifier map {v => "#Remark: " + v}

  def simpleIdentifier    = oneOf("date", "time-taken", "time", "bytes", "cached")

  def fieldDirective      = oneOf(simpleIdentifier, prefixedIdentifier, headerIdentifier, customIdentifier)

  def headerIdentifier    = {
    for {
      prefix  <- prefix
      header  <- identifier
    } yield (prefix + "(" + header + ")")
  }
  def customIdentifier    = identifier map {v => "x-" + v}

  def prefix              = oneOf("c", "s", "r", "cs", "sc", "sr", "rs")

  def prefixedIdentifier  = {
    for {
      prefix    <- prefix
      prefixed  <- oneOf("ip", "dns", "status", "comment", "method", "uri", "uri-stem", "uri-query")
    } yield (prefix + "-" + prefixed)
  }

  def dateTime = {
    for{
      dd   <- choose(10, 28)
      MM   <- choose(10, 12)
      yyyy <- listOfN(4, numChar) map {v => v.mkString("")}
      HH   <- choose(10, 23)
      mm   <- choose(10, 59)
      ss   <- choose(10, 59)
    } yield (dd + "-" + MM + "-" + yyyy +" " + HH + ":" + mm + ":" + ss)
  }

}