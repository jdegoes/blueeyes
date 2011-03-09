package blueeyes.util.logging

import org.specs.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import java.io.{FileInputStream, File}
import RollPolicies._
import org.apache.commons.io.IOUtils

class W3ExtendedLoggerSpec extends Specification{
  private val directives = FieldsDirective(List(DateIdentifier, TimeIdentifier))

  private var w3Logger: W3ExtendedLogger = _
  "W3ExtendedLogger" should {
    doLast {
      w3Logger.close

      new File(w3Logger.fileName.get).delete
    }
    "creates log file" in {
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + "w3.log", Never, directives, 1)

      new File(w3Logger.fileName.get).exists must be (true)
    }
    "init log file" in {
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + "w3.log", Never, directives, 1)

      val content = getContents(new File(w3Logger.fileName.get))

      content.indexOf("#Version: 1.0")      must notEq (-1)
      content.indexOf("#Date: ")            must notEq (-1)
      content.indexOf(directives.toString)  must notEq (-1)
    }

    "write log entries" in {
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + "w3.log", Never, directives, 1)

      w3Logger("foo")
      w3Logger("bar")

      Thread.sleep(2000)

      w3Logger("baz")

      val content = getContents(new File(w3Logger.fileName.get))

      content.indexOf("foo") must notEq (-1)
      content.indexOf("bar") must notEq (-1)
    }
  }

  private def getContents(file: File) = {
    val inputStream = new FileInputStream(file)
    try {
      IOUtils.toString(inputStream, "UTF-8")
    }
    finally inputStream.close
  }
}