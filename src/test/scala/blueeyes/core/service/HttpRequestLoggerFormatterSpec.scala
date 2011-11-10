package blueeyes.core.service

import org.specs2.mutable.Specification
import blueeyes.parsers.W3ExtendedLogAST.{FieldIdentifier, FieldsDirective}

class HttpRequestLoggerFormatterSpec extends Specification{
  "HttpRequestLoggerFormatter" should{
    "create w3c formatter" in{
      HttpRequestLoggerFormatter("w3c").isInstanceOf[HttpRequestLoggerW3CFormatter] must_==(true)
    }
    "create json formatter" in{
      HttpRequestLoggerFormatter("json").isInstanceOf[HttpRequestLoggerJsonFormatter] must_==(true)
    }
    "create custom formatter" in{
      HttpRequestLoggerFormatter("blueeyes.core.service.HttpRequestLoggerFormatterImpl").isInstanceOf[HttpRequestLoggerFormatterImpl] must_==(true)
    }
  }
}

class HttpRequestLoggerFormatterImpl extends HttpRequestLoggerFormatter{
  def formatLog(log: List[(FieldIdentifier, Either[String, Array[Byte]])]) = ""

  def formatHeader(fieldsDirective: FieldsDirective) = ""
}