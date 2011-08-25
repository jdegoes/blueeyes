package blueeyes.core.service

import org.apache.commons.codec.binary.Base64
import blueeyes.parsers.W3ExtendedLogAST.{FieldsDirective, FieldIdentifier}
import java.text.SimpleDateFormat
import java.util.Date

class HttpRequestLoggerW3CFormatter extends HttpRequestLoggerFormatter{
  def formatLog(entry: List[(FieldIdentifier, Either[String, Array[Byte]])]) = {
    val values: List[String] = entry.map{_ match{
      case (_, Right(value)) => (encodeBase64(value))
      case (_, Left(value)) => value
    }}
    values.mkString(" ")
  }


  def formatHeader(fieldsDirective: FieldsDirective) =
    new StringBuilder("#Version: 1.0\n").append("#Date: %s\n".format(new SimpleDateFormat("dd-MMM-yyyy HH:MM:SS").format(new Date()))).append(fieldsDirective.toString).toString()

  private def encodeBase64(value: Array[Byte]) = "\"" + new String(Base64.encodeBase64(value), "UTF-8") + "\""
}