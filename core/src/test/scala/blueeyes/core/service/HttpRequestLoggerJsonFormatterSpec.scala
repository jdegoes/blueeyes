package blueeyes.core.service

import org.specs2.mutable.Specification
import blueeyes.json._
import blueeyes.parsers.W3ExtendedLogAST._

class HttpRequestLoggerJsonFormatterSpec extends Specification{
  private val formatter = new HttpRequestLoggerJsonFormatter()
  "HttpRequestLoggerJsonFormatter" should{
    "create valid json for values without prefix" in{
      val values: List[(FieldIdentifier, Either[String, Array[Byte]])] = (TimeIdentifier, Left("11:11:2001")) :: (CachedIdentifier, Left("1")) :: Nil
      formatter.formatLog(values) must_==(format(fields(values), Nil, Nil))
    }
    "create valid json for values with client prfix" in{
      val values: List[(FieldIdentifier, Either[String, Array[Byte]])] = (MethodIdentifier(ClientToServerPrefix), Left("GET")) :: (UriIdentifier(ClientToServerPrefix), Left("/foo/bar")) :: Nil
      formatter.formatLog(values) must_==(format(Nil, fields(values), Nil))
    }
    "create valid json for values with server prfix for binary data" in{
      val values: List[(FieldIdentifier, Either[String, Array[Byte]])] = (ContentIdentifier(ServerToClientPrefix), Right("content".getBytes("UTF-8"))) :: Nil
      formatter.formatLog(values) must_==(format(Nil, Nil, fields(values)))
    }
    "create valid json for values with server prfix" in{
      val values: List[(FieldIdentifier, Either[String, Array[Byte]])] = (StatusIdentifier(ServerToClientPrefix), Left("OK")) :: (UriIdentifier(ServerToClientPrefix), Left("/foo/bar")) :: Nil
      formatter.formatLog(values) must_==(format(Nil, Nil, fields(values)))
    }
  }
  private def format(noPrefix: List[JField], clientPrefix: List[JField], serverPrefix: List[JField]) = {
    val json = JObject(noPrefix ::: JField("request", JObject(clientPrefix)) :: JField("response", JObject(serverPrefix)) :: Nil)
    json.renderCompact
  }
  private def fields(values: List[(FieldIdentifier, Either[String, Array[Byte]])]) = toStringValue(values).map(v => JField(v._1.toString, JString(v._2)))
  private def  toStringValue(entry: List[(FieldIdentifier, Either[String, Array[Byte]])]) = entry.map{_ match{
    case (identifier, Right(value)) => (identifier, new String(value, "UTF-8"))
    case (identifier, Left(value))  => (identifier, value)
  }}
}