package blueeyes.core.service

import blueeyes.parsers.W3ExtendedLogAST.{FieldsDirective, FieldIdentifier}

trait HttpRequestLoggerFormatter{
  def formatLog(log: List[(FieldIdentifier, Either[String, Array[Byte]])]): String

  def formatHeader(fieldsDirective: FieldsDirective): String
}

object HttpRequestLoggerFormatter{
  def apply(logName: String) = logName match {
    case "w3c"  => new HttpRequestLoggerW3CFormatter()
    case "json" => new HttpRequestLoggerJsonFormatter()
    case _      => Class.forName(logName).newInstance().asInstanceOf[HttpRequestLoggerFormatter]
  }
}