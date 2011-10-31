package blueeyes.core.service

import org.specs.Specification
import blueeyes.core.http.HttpRequest
import blueeyes.util.printer.HtmlPrinter
import net.lag.configgy.Config

class ServiceDocumenterSpec extends Specification with HttpRequestHandlerCombinators{
  import Metadata._

  "ServiceDocumente" should{
    "create service docuementation" in{
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
<title>REST API Resources | Foo (1.0.0)</title>  </head>

  <body>
    <div class = "root">
    <h1>REST API Resources | Foo (1.0.0)</h1>

    <table>
      <tbody>
        <tr><td>Sample service</td></tr>
      </tbody>
    </table>


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

      implicit val printer = HtmlPrinter
      val handler : HttpService[Int, Int] = {
        path("/details", Some("Personal ")) {
          path("/bar") {
            path("/john", Some("john details")) {
              get { (request: HttpRequest[Int]) => 0 }
            }
          }~
          path("/kate", Some("kate details")) {
            get { (request: HttpRequest[Int]) => 0 }
          }
        }
      }
      ServiceDocumenter.printFormatted(ServiceContext(new Config(), "Foo", ServiceVersion(1, 0, "0"), Some("Sample service"), "localhost", 8080, 8081), handler) mustEqual (expected)
    }
  }
}