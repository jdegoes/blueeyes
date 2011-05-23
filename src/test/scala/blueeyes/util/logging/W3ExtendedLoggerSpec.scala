package blueeyes.util.logging

import org.specs.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import RollPolicies._
import java.io.{ByteArrayOutputStream, FileInputStream, File}

class W3ExtendedLoggerSpec extends Specification{
  private val directives = FieldsDirective(List(DateIdentifier, TimeIdentifier))

  private var w3Logger: W3ExtendedLogger = _
  "W3ExtendedLogger" should {
    "creates log file" in {
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3.log", Never, directives, 1)

      new File(w3Logger.fileName.get).exists must be (true)
      cleanUp()
    }
    "init log file" in {
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3_1.log", Never, directives, 1)

      val content = getContents(new File(w3Logger.fileName.get))
      cleanUp()

      content.indexOf("#Version: 1.0")      must notEq (-1)
      content.indexOf("#Date: ")            must notEq (-1)
      content.indexOf(directives.toString)  must notEq (-1)
    }

    "flush entries while closing" in{
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3_2.log", Never, directives, 1)

      w3Logger("foo")
      w3Logger("bar")

      val future = w3Logger.close
      future.value must eventually (beSomething)

      val content = getContents(new File(w3Logger.fileName.get))
      cleanUp()

      content.indexOf("foo") must notEq (-1)
      content.indexOf("bar") must notEq (-1)
    }

    "write log entries" in {
      w3Logger = W3ExtendedLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3_3.log", Never, directives, 1)

      w3Logger("foo")
      w3Logger("bar")
      w3Logger("baz")

      val file = new File(w3Logger.fileName.get)

      getContents(file).indexOf("foo") must eventually(notEq (-1))
      getContents(file).indexOf("bar") must eventually(notEq (-1))

      cleanUp()
    }

    def cleanUp(){
      val future = w3Logger.close
      future.value must eventually (beSomething)

      new File(w3Logger.fileName.get).delete
    }
  }

  private def getContents(file: File) = {
    val inputStream = new FileInputStream(file)
    try {

      val byteContents  = new ByteArrayOutputStream()
      val buffer        = new Array[Byte](1024)
      var bytesRead     = 0

      while (bytesRead != -1) {
        bytesRead = inputStream.read(buffer)
        if (bytesRead != -1) {
          byteContents.write(buffer, 0, bytesRead)
        }
      }

      byteContents.flush()

      new String(byteContents.toByteArray(), "UTF-8")
    }
    finally inputStream.close
  }
}
