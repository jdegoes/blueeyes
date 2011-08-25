package blueeyes.core.service

import org.specs.Specification
import org.apache.commons.codec.binary.Base64
import blueeyes.parsers.W3ExtendedLogAST._

class HttpRequestLoggerW3CFormatterSpec extends Specification {
  private val formatter = new HttpRequestLoggerW3CFormatter()
  "HttpRequestLoggerW3CFormatter" should{
    "format values in one line" in{
      formatter.formatLog((MethodIdentifier(ClientToServerPrefix), Left("GET")) :: (UriIdentifier(ClientToServerPrefix), Left("/foo/bar")) :: Nil) must beEqual("GET /foo/bar")
    }
    "format content using Base64 encoding" in{
      val encoded = formatter.formatLog(Tuple2[FieldIdentifier, Either[String, Array[Byte]]](ContentIdentifier(ClientToServerPrefix), Right("content".getBytes("UTF-8"))) :: Nil)
      decodeBase64(encoded) must beEqual("content")
    }
  }

  private def decodeBase64(data: String) = new String(Base64.decodeBase64(data.substring(1, data.length - 1)), "UTF-8")

}