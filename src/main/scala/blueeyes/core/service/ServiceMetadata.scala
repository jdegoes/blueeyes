package blueeyes
package core.service

import blueeyes.core.http._
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._

import scalaz.Scalaz._
import scalaz.{Success, Validation, Failure, Semigroup}

sealed trait NotServed {
  def or[A](result: => Validation[NotServed, A]): Validation[NotServed, A]
}

case class DispatchError(exception: HttpException) extends NotServed {
  override def or[A](result: => Validation[NotServed, A]) = this.fail[A]
}

object DispatchError {
  def apply(failure: HttpFailure, reason: String): DispatchError = DispatchError(HttpException(failure, reason))
  def apply(t: (HttpFailure, String)): DispatchError = DispatchError(HttpException(t._1, t._2))
}

case class Inapplicable private[service] (services: AnyService*) extends NotServed {
  override def or[A](result: => Validation[NotServed, A]) = result match {
    case Failure(Inapplicable(others @ _*)) => Inapplicable(services ++ others: _*).fail[A]
    case other => other
  }
}

sealed abstract class Metadata(private val sortOrder: Int) extends Ordered[Metadata] {
  def compare(other: Metadata) = sortOrder compare other.sortOrder
}

case class PathPatternMetadata(pattern: RestPathPattern, desc: Option[String] = None) extends Metadata(1) {
  override def compare(other: Metadata) = other match {
    case PathPatternMetadata(p, _) => pattern.toString.compare(p.toString)
    case _ => super.compare(other)
  }
}

case class HttpMethodMetadata(method: HttpMethod) extends Metadata(2) {
  override def compare(other: Metadata) = other match {
    case HttpMethodMetadata(m) => method.toString.compare(m.toString)
    case _ => super.compare(other)
  }
}

case class HeaderMetadata[T <: HttpHeader](header: Either[HttpHeaderField[T], T], default: Option[T] = None, desc: Option[String] = None) extends Metadata(3) {
  override def compare(other: Metadata) = other match {
    case HeaderMetadata(h, _, _) => (header, h) match {
      case (Left(h1),  Left(h2))  => h1.name.compare(h2.name)
      case (Right(h1), Right(h2)) => h1.name.compare(h2.name)
      case (Left(_), _) => -1
      case _ => 1
    }

    case _ => super.compare(other)
  }
}

case class ParameterMetadata(parameter: Symbol, default: Option[String], desc: Option[String] = None) extends Metadata(4) {
  override def compare(other: Metadata) = other match {
    case ParameterMetadata(p, _, _) => parameter.toString.compare(p.toString)
    case _ => super.compare(other)
  }
}

case class CookieMetadata(ident: Symbol,     default: Option[String], desc: Option[String] = None) extends Metadata(5) {
  override def compare(other: Metadata) = other match {
    case CookieMetadata(i, _, _) => ident.toString.compare(i.toString)
    case _ => super.compare(other)
  }
}

case class DescriptionMetadata  (description: String) extends Metadata(6) 
case class DataSizeMetadata     (dataSize: DataSize)  extends Metadata(7)
case class EncodingMetadata     (encodings: Encoding*)  extends Metadata(8)
case class AndMetadata          (metadata: Metadata*) extends Metadata(9)
case class OrMetadata           (metadata: Metadata*) extends Metadata(10)

object Metadata {
  implicit object MetadataSemigroup extends Semigroup[Metadata] {
    private def paths(m: Metadata*) = m.foldLeft((List.empty[PathPatternMetadata], List.empty[Metadata])) {
      case ((p, m), pathm : PathPatternMetadata) => (pathm :: p, m)
      case ((p, m), om) => (p, om :: m)
    }

    def append(a: Metadata, b: => Metadata) = {
      val (ap, am) = a match {
        case AndMetadata(ams @ _*) => paths(ams: _*)
        case other => paths(other)
      }

      val (bp, bm) = b match {
        case AndMetadata(bms @ _*) => paths(bms: _*)
        case other => paths(other)
      }

      AndMetadata((for (p1 <- ap; p2 <- bp) yield PathPatternMetadata(p1.pattern / p2.pattern, None)) ++ am ++ bm: _*)
    }
  }  

  implicit object StringFormatter extends Formatter[Metadata, String] {
    def format(m: Metadata) = m match {
      case PathPatternMetadata(pattern, desc) => Value("Service path: " + pattern.toString + desc.str(": " + _))
      case HttpMethodMetadata(method) => Value("HTTP method: " + method.toString)
      case HeaderMetadata(header, _, desc)    => Value("Header expected: " + header.fold(_.name, _.header) + desc.str(" (" + _ + ")"))

      case ParameterMetadata(parameter, default, desc) => 
        Value("Request Parameter: " + parameter + desc.str(": " + _ ) 
              + default.map("; a default of " + _ + " will be used if no value is specified.").getOrElse(" (required)"))

      case CookieMetadata(parameter, default, desc) => 
        Value("HTTP cookie: " + parameter + desc.str(": " + _ ) 
              + default.map("; a default of " + _ + " will be used if no value is specified.").getOrElse(" (required)"))

      case EncodingMetadata(encodings @ _*) => 
        Append(
          Value("The following encodings are supported:"), Break, 
          Nest(Append(encodings.map(e => Value(e.toString): Printable[String]).toList.intersperse(Break): _*)))

      case DescriptionMetadata(desc) => Value(desc)

      case OrMetadata(metadata @ _*) => 
        Append(
          Seq(Value("You may use any one of the following configurations: "), Break) ++ 
          metadata.map(m => Nest(format(m)): Printable[String]).toList.intersperse(Append(Break, Value("or"), Break)): _*)

      case AndMetadata(metadata @ _*) => Append(metadata.sorted.map(format).toList.intersperse(Break): _*)

      case _ => Empty
    }
  }
}


// vim: set ts=4 sw=4 et:
