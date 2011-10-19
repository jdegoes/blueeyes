package blueeyes.util.printer

import scala.collection.mutable.StringBuilder

sealed trait Printable[+T] {
  def ~[TT >: T](other: Printable[TT]) = Append(this, other)
}
case object Empty extends Printable[Nothing]
case object Break extends Printable[Nothing]
case class Value[T](value: T) extends Printable[T]
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
      case Value(str) => buffer.append(str).append(" ")
      case Append(ps @ _*) => ps.foldLeft(buffer)((b, p) => append(p, b))
      case Nest(child) => buffer.append(append(child, new StringBuilder()).linesWithSeparators.map("  " + _).mkString)
    }

    append(printable, new StringBuilder()).toString
  }
}


// vim: set ts=4 sw=4 et:
