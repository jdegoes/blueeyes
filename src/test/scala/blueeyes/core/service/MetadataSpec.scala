package blueeyes.core.service

import org.specs2.mutable.Specification

import blueeyes.core.http._
import blueeyes.util.printer._
import blueeyes.core.http.HttpMethods._
import RestPathPatternParsers._
import akka.dispatch.Future
import blueeyes.bkka.AkkaDefaults

class MetadataSpec extends Specification with HttpRequestHandlerCombinators with AkkaDefaults {
  import Metadata._

  "serviceToMetadata" should{
    "extract metadata from services" in{
      val expected = """<html>

  <head>
      <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
      <style>
        body         {line-height: 1;font: 13px/1.5 'Helvetica Neue',Arial,'Liberation Sans',FreeSans,sans-serif;color: #555;}
        td           {padding: 6px 0 6px 0;vertical-align: top;}
        table        {width:100%}
        div.nested   {padding-bottom: 10px; padding-top: 10px; padding-left: 50px; border-bottom: 1px solid #EEE;}
        div.root     {padding-left: 50px;padding-top: 10px;padding-right: 50px;}
        td.types     {width: 15%;}
        td.values    {width: 25%}
        td.desc      {width: 60%;}
        .font-values {color: #0094C2;}
        .font-types, .caption {font-weight: bold;}
        h1, .caption {color: #333;}
        h1           {font-size: 3em;font-weight: 300;margin: 0;padding: 0;}
        div.desc     {color: #777;margin-top: 0.5em}
      </style>
  </head>

  <body>
    <div class = "root">
    <div class="nested">
        <table>
          <tbody>
            <tr>
                <td class="types caption">Parameter Type</td>
                <td class="values caption">Parameter</td>
                <td class="desc caption">Description</td>
            </tr>
          </tbody>
        </table></div>
<div class="nested">
        <table>
          <tbody>
            <tr>
                <td class="types font-types">Service path</td>
                <td class="values font-values">/details/bar/john</td>
                <td class="desc">Personal john details</td>
            </tr>
          </tbody>
        </table>

        <table>
          <tbody>
            <tr>
                <td class="types font-types">HTTP method</td>
                <td class="values font-values">GET</td>
                <td class="desc"></td>
            </tr>
          </tbody>
        </table></div>

<div class="nested">
        <table>
          <tbody>
            <tr>
                <td class="types font-types">Service path</td>
                <td class="values font-values">/details/kate</td>
                <td class="desc">Personal kate details</td>
            </tr>
          </tbody>
        </table>

        <table>
          <tbody>
            <tr>
                <td class="types font-types">HTTP method</td>
                <td class="values font-values">GET</td>
                <td class="desc"></td>
            </tr>
          </tbody>
        </table></div>
     </div>
  </body>
</html>"""

      val handler : HttpService[Int, Future[HttpResponse[Int]]] = {
        path("/details") {
          path("/bar") {
            describe("Personal john details"){
              path("/john") {
                get{ (request: HttpRequest[Int]) => Future(HttpResponse[Int](content = Some(1))) }
              }
            }
          }~
          describe("Personal kate details"){
            path("/kate") {
              get{ (request: HttpRequest[Int]) => Future(HttpResponse[Int](content = Some(0))) }
            }
          }
        }
      }
      HtmlPrinter.printFormatted(serviceToMetadata(handler)) mustEqual (expected)
    }
  }
}
