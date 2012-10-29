package blueeyes.util.printer

import blueeyes._
import scala.collection.mutable.StringBuilder

sealed trait Printable[+T] {
  def ~[TT >: T](other: Printable[TT]) = Append(this, other)
}
case object Empty extends Printable[Nothing]
case object Break extends Printable[Nothing]
case class Value[T](valueType: String, value: T, desc: Option[String]) extends Printable[T]
case class ValueCaption(typeCaption: String, valueCaption: String, descCaption: String) extends Printable[Nothing]
case class Title(value: String) extends Printable[Nothing]
case class Description(value: String) extends Printable[Nothing]
case class Append[T](printables: Printable[T]*) extends Printable[T]
case class Nest[T](child: Printable[T]) extends Printable[T]

trait Formatter[-A, +B] {
  def format(a: A): Printable[B]
}

trait Printer[A] {
  def print(printable: Printable[A]): A
  def printFormatted[B](b: B)(implicit formatter: Formatter[B, A]) = print(formatter.format(b))
}

object SimpleStringPrinter extends Printer[String] {
  def print(printable: Printable[String]): String = {
    def append(printable: Printable[String], buffer: StringBuilder): StringBuilder = printable match {
      case Empty => buffer
      case Break => buffer.append("\n")
      case Title(str) => buffer.append(str)
      case Description(str) => buffer.append("  ").append(str)
      case ValueCaption(typeCaption, valueCaption, descCaption) => columns(typeCaption, valueCaption, descCaption, buffer).append('\n').append(Array.fill(60)('-').mkString(""))
      case Value(valueType, str, desc) => columns(valueType, str, desc.getOrElse(""), buffer)
      case Append(ps @ _*) => ps.foldLeft(buffer)((b, p) => append(p, b))
      case Nest(child) => buffer.append(append(child, new StringBuilder()).linesWithSeparators.map("  " + _).mkString)
    }

    def columns(value1: String, value2: String, value3: String, buffer: StringBuilder) = buffer.append(value1).append(column(value2, 25, value1.length)).append(column(value3, 25, value2.length))
    def column(value: String, position: Int, shift: Int) = if(value.length > 0) Array.fill(position - shift)(' ').mkString("") + value else ""

    append(printable, new StringBuilder()).toString
  }
}

object HtmlPrinter extends Printer[String] {
  def print(printable: Printable[String]): String = {
    def appendBody(printable: Printable[String], buffer: StringBuilder): StringBuilder = printable match {
      case Empty => buffer
      case Break => buffer.append("\n")
      case Title(str) => buffer.append("""<h1>%s</h1>""".format(str))
      case Description(str) =>
        buffer.append("""    <table>
      <tbody>
        <tr><td>%s</td></tr>
      </tbody>
    </table>
""".format(str))
      case Value(valueType, str, desc) =>
        buffer.append("""
        <table>
          <tbody>
            <tr>
                <td class="types font-types">%s</td>
                <td class="values font-values">%s</td>
                <td class="desc">%s</td>
            </tr>
          </tbody>
        </table>""".format(valueType, str, desc.getOrElse("")))
      case Append(ps @ _*) => ps.foldLeft(buffer){(b, p) =>
        appendBody(p, b)
      }
      case ValueCaption(typeCaption, valueCaption, descCaption) =>
        buffer.append("""
        <table>
          <tbody>
            <tr>
                <td class="types caption">%s</td>
                <td class="values caption">%s</td>
                <td class="desc caption">%s</td>
            </tr>
          </tbody>
        </table>""".format(typeCaption, valueCaption, descCaption))
      case Nest(child) =>
        buffer.append("""<div class="nested">%s</div>""".format(appendBody(child, new StringBuilder())))
    }

    def appendHeader(printable: Printable[String], buffer: StringBuilder): StringBuilder = printable match {
      case Title(str)      => buffer.append("""<title>%s</title>""".format(str))
      case Append(ps @ _*) => ps.foldLeft(buffer){(b, p) => appendHeader(p, b) }
      case Nest(child)     => buffer.append(appendHeader(child, new StringBuilder()).linesWithSeparators.map("  " + _).mkString)
      case _ => buffer
    }

    val body = """
  <body>
    <div class = "root">
    %s
     </div>
  </body>
""".format(appendBody(printable, new StringBuilder()).toString)

    val header = """
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
""" + appendHeader(printable, new StringBuilder()) + "  </head>\n"

    "<html>\n" + header + body + "</html>"
  }
}


// vim: set ts=4 sw=4 et:
