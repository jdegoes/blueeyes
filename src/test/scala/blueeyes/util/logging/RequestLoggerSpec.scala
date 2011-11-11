package blueeyes.util.logging

import org.specs2.mutable.Specification
import blueeyes.parsers.W3ExtendedLogAST._
import RollPolicies._
import java.io.File
import scala.io.Source
import blueeyes.core.service.HttpRequestLoggerW3CFormatter
import org.specs2.specification.AfterExample

class RequestLoggerSpec extends Specification with AfterExample{
  private val directives = FieldsDirective(List(DateIdentifier, TimeIdentifier))
  private val formatter  = new HttpRequestLoggerW3CFormatter()

  override def is = args(sequential = true) ^ super.is

  private var w3Logger: RequestLogger = _
  "W3ExtendedLogger" should {
    def header() = formatter.formatHeader(directives)

    "creates log file" in {
      w3Logger = RequestLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3.log", Never, header _, 1)

      new File(w3Logger.fileName.get).exists must be_==(true)
    }
    "init log file" in {
      w3Logger = RequestLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3_1.log", Never, header _, 1)

      val content = getContents(new File(w3Logger.fileName.get))

      content.indexOf("#Version: 1.0")      must_!= (-1)
      content.indexOf("#Date: ")            must_!= (-1)
      content.indexOf(directives.toString)  must_!= (-1)
    }

    "flush entries while closing" in{
      w3Logger = RequestLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3_2.log", Never, header _, 1)

      w3Logger("foo")
      w3Logger("bar")

      val future = w3Logger.close
      future.value must eventually (beSome)

      val content = getContents(new File(w3Logger.fileName.get))

      content.indexOf("foo") must_!= (-1)
      content.indexOf("bar") must_!= (-1)
    }

    "write log entries" in {
      w3Logger = RequestLogger.get(System.getProperty("java.io.tmpdir") + File.separator + "w3_3.log", Never, header _, 1)

      w3Logger("foo")
      w3Logger("bar")
      w3Logger("baz")

      val file = new File(w3Logger.fileName.get)

      getContents(file).indexOf("foo") must eventually(not (be_== (-1)))
      getContents(file).indexOf("bar") must eventually(not (be_== (-1)))
    }
  }

  def cleanUp(){
    val future = w3Logger.close
    future.value must eventually (beSome)

    new File(w3Logger.fileName.get).delete
  }

  protected def after = cleanUp()

  private def getContents(file: File) = Source.fromFile(file, "UTF-8").getLines.mkString("\n")
}
