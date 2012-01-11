package blueeyes.core.service

import blueeyes.json.JsonAST.{JString, JField, JObject}
import blueeyes.json.Printer
import blueeyes.parsers.W3ExtendedLogAST._

class HttpRequestLoggerJsonFormatter extends HttpRequestLoggerFormatter{
  def formatLog(entry: List[(FieldIdentifier, Either[String, Array[Byte]])]) = {
    def fields(values: List[(FieldIdentifier, String)]) = values.map(v => JField(v._1.toString, JString(v._2)))

    val values: List[(FieldIdentifier, String)] = entry.map{_ match{
      case (identifier, Right(value)) => (identifier, new String(value, "UTF-8"))
      case (identifier, Left(value))  => (identifier, value)
    }}

    val (noPrefix, withPrefix)       = values.partition{
      case (e: PredefinedIdentifierNoPrefix, value) => true
      case _ => false
    }
    val (clientPrefix, serverPrefix) = withPrefix.partition{
      case (e: PredefinedIdentifierPrefix, value) if (e.prefix == ClientPrefix || e.prefix == ClientToServerPrefix) => true
      case _ => false
    }

    val json = JObject(fields(noPrefix) ::: JField("request", JObject(fields(clientPrefix))) :: JField("response", JObject(fields(serverPrefix))) :: Nil)
    Printer.compact(Printer.render(json))
  }

  def formatHeader(fieldsDirective: FieldsDirective) = ""
}