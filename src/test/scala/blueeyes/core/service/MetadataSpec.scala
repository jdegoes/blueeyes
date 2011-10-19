package blueeyes.core.service

import blueeyes.core.http._
import blueeyes.util.printer._

import org.specs.Specification

class MetadataSpec extends Specification {
  "pretty-printing service metadata" should {
    "pretty-print a simple set of metadata" in {

      val expected = """You may use any one of the following configurations:  
  HTTP method: GET 
  Request Parameter: 'callback: A callback method identifier is required when using JsonP with a "GET" request. (required) 
or 
  HTTP method: POST 
or 
  HTTP method: PUT 
or 
  HTTP method: DELETE """

      SimpleStringPrinter.printFormatted(OrMetadata(
        AndMetadata(
          HttpMethodMetadata(HttpMethods.GET),
          ParameterMetadata('callback, None, Some("A callback method identifier is required when using JsonP with a \"GET\" request."))
        ),
        HttpMethodMetadata(HttpMethods.POST),
        HttpMethodMetadata(HttpMethods.PUT),
        HttpMethodMetadata(HttpMethods.DELETE)
      )) must_== expected
    }
  }
}

// vim: set ts=4 sw=4 et:
