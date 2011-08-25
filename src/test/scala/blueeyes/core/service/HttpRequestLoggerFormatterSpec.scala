package blueeyes.core.service

import org.specs.Specification
import blueeyes.parsers.W3ExtendedLogAST.{FieldIdentifier, FieldsDirective}

class HttpRequestLoggerFormatterSpec extends Specification {
  "HttpRequestLoggerFormatter" should{
    "create w3c formatter" in{
      HttpRequestLoggerFormatter("w3c").isInstanceOf[HttpRequestLoggerW3CFormatter] must be (true)
    }
    "create json formatter" in{
      HttpRequestLoggerFormatter("json").isInstanceOf[HttpRequestLoggerJsonFormatter] must be (true)
    }
    "create custom formatter" in{
      HttpRequestLoggerFormatter("blueeyes.core.service.HttpRequestLoggerFormatterImpl").isInstanceOf[HttpRequestLoggerFormatterImpl] must be (true)
    }
  }
}

class HttpRequestLoggerFormatterImpl extends HttpRequestLoggerFormatter{
  def formatLog(log: List[(FieldIdentifier, Either[String, Array[Byte]])]) = ""

  def formatHeader(fieldsDirective: FieldsDirective) = ""
}