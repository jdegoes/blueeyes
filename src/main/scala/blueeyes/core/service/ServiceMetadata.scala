package blueeyes
package core.service

import blueeyes.core.http._
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._
import blueeyes.core.http.HttpHeaders.{`Content-Type`}

import scalaz.Scalaz._
import scalaz.{Validation, Failure, Semigroup}

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

case class PathPatternMetadata(pattern: RestPathPattern) extends Metadata(1) {
  override def compare(other: Metadata) = other match {
    case PathPatternMetadata(p) => pattern.toString.compare(p.toString)
    case _ => super.compare(other)
  }
}

case class HttpMethodMetadata(method: HttpMethod) extends Metadata(2) {
  override def compare(other: Metadata) = other match {
    case HttpMethodMetadata(m) => method.toString.compare(m.toString)
    case _ => super.compare(other)
  }
}

sealed abstract class HeaderMetadata[T <: HttpHeader](sortOrder: Int) extends Metadata(sortOrder) {
  override def compare(other: Metadata) = other match {
    case h: HeaderMetadata[_] => (header, h.header) match {
      case (Left(h1),  Left(h2))  => h1.name.compare(h2.name)
      case (Right(h1), Right(h2)) => h1.name.compare(h2.name)
      case (Left(_), _) => -1
      case _ => 1
    }

    case _ => super.compare(other)
  }
  def header: Either[HttpHeaderField[T], T]

  def default: Option[T]
}

case class RequestHeaderMetadata[T <: HttpHeader](header: Either[HttpHeaderField[T], T], default: Option[T] = None) extends HeaderMetadata[T](3)

case class ResponseHeaderMetadata[T <: HttpHeader](header: Either[HttpHeaderField[T], T], default: Option[T] = None) extends HeaderMetadata[T](4)

case class ParameterMetadata(parameter: Symbol, default: Option[String]) extends Metadata(5) {
  override def compare(other: Metadata) = other match {
    case ParameterMetadata(p, _) => parameter.toString.compare(p.toString)
    case _ => super.compare(other)
  }
}

case class CookieMetadata(ident: Symbol,     default: Option[String]) extends Metadata(6) {
  override def compare(other: Metadata) = other match {
    case CookieMetadata(i, _) => ident.toString.compare(i.toString)
    case _ => super.compare(other)
  }
}

case class TitleMetadata        (title: String) extends Metadata(7)
case class DescriptionMetadata  (description: String) extends Metadata(8)
case class DataSizeMetadata     (dataSize: DataSize)  extends Metadata(9)
case class EncodingMetadata     (encodings: Encoding*)  extends Metadata(10)
case class AndMetadata          (metadata: Metadata*) extends Metadata(11)
case class OrMetadata           (metadata: Metadata*) extends Metadata(12)

object Metadata {
  implicit object MetadataSemigroup extends Semigroup[Metadata] {
    private def paths(m: Metadata*) = m.foldLeft((List.empty[(Option[DescriptionMetadata], PathPatternMetadata)], List.empty[Metadata])) {
      case ((p, (d @ DescriptionMetadata(desc)) :: m), pathm : PathPatternMetadata) => ((Some(d), pathm) :: p, m)
      case ((p, m), pathm : PathPatternMetadata) => ((None, pathm) :: p, m)
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

      val pm = ap ::: bp match {
        case Nil => List[Metadata]()
        case abp =>
          val (desc, path) = abp.tail.foldLeft(abp.head){case ((md, mp), (d, p)) => (md.map(_.description) ⊹ d.map(_.description) map (DescriptionMetadata(_)),  PathPatternMetadata(mp.pattern ~ p.pattern))}
          desc.toList ::: List(path)
      }
      AndMetadata(am ++ bm ++ pm: _*)
    }
  }  

  implicit object StringFormatter extends Formatter[Metadata, String] {
    def format(m: Metadata) = formatMetadata(m)

    def formatMetadata(m: Metadata): Printable[String] = m match {
      case PathPatternMetadata(pattern) => Value("Service path", pattern.toString, None)
      case HttpMethodMetadata(method)         => Value("HTTP method", method.toString, None)

      case RequestHeaderMetadata(header, _)  => header match{
        case Right(value : `Content-Type`) => Value("Accept", value.value, None)
        case _ => Value("Request Header", header.fold(_.name, _.header), None)
      }
      case ResponseHeaderMetadata(header, _) => header match{
        case Right(value : `Content-Type`) => Value("Produce", value.value, None)
        case _ => Value("Response Header", header.fold(_.name, _.header), None)
      }

      case ParameterMetadata(parameter, default) =>
        Value("Request Parameter", parameter.toString, Some(default.map("; a default of " + _ + " will be used if no value is specified.").getOrElse(" (required)")))

      case CookieMetadata(parameter, default) =>
        Value("HTTP cookie", parameter.toString, Some(default.map("; a default of " + _ + " will be used if no value is specified.").getOrElse(" (required)")))

      case EncodingMetadata(encodings @ _*) => Value("Supported encodings", encodings.mkString(", "), None)
      case DescriptionMetadata(desc) => Append(Description(desc), Break)
      case TitleMetadata(title) => Append(Title(title), Break)

      case OrMetadata(metadata @ _*) =>
        Append(Seq(Nest(ValueCaption("Parameter Type", "Parameter", "Description")), Break) ++
        metadata.map(m => Nest(formatMetadata(m)): Printable[String]).toList.intersperse(Append(Break, Break)): _*)

      case AndMetadata(metadata @ _*) => metadata match{
        case Seq(metadata, DescriptionMetadata(desc)) => formatMetadata(metadata) match {
          case Value(valueType, value, valueDesc) => Value(valueType, value, Some(desc + valueDesc.getOrElse("")))
          case p => Append(Description(desc), Break, p)
        }
        case _ =>
          val grouped = metadata.reverse.foldLeft(List[Metadata]()) {
            case (x :: xs, d: DescriptionMetadata) => AndMetadata(x, d) :: xs
            case (xs, m) => m :: xs
          }
          val sorted = grouped.sortWith{
            case (AndMetadata(metadata @ _, DescriptionMetadata(desc)), m) => metadata.compare(m) < 0
            case (m, AndMetadata(metadata @ _, DescriptionMetadata(desc))) => m.compare(metadata) < 0
            case (m1, m2) => m1.compare(m2) < 0
          }
          Append(sorted.map(formatMetadata).toList.intersperse(Break): _*)
      }

      case _ => Empty
    }
  }

  def serviceToMetadata(service: AnyService): Metadata = {
    def metadata(m: Option[Metadata], service: AnyService): Option[Metadata] = service match {
      case OrService(services @ _*)  =>
        val ms = services.map(metadata(m, _)).collect{case Some(v) => v}
        Some(OrMetadata(ms.foldLeft(Seq[Metadata]()){
          case (l, OrMetadata(ms @ _*)) => l ++ ms
          case (l, m) => l :+ m
        }: _*))
      case s: DelegatingService[_, _, _, _] => metadata(m ⊹ nePath(s.metadata), s.delegate)
      case s => m ⊹ nePath(s.metadata)
    }

    def nePath(m: Option[Metadata]) = m.flatMap{
      case PathPatternMetadata(RestPathPatternParsers.EmptyPathPattern) => None
      case _ => m
    }

    metadata(None, service) match {
      case Some(m: OrMetadata) => m
      case Some(m) => OrMetadata(m)
      case None => OrMetadata()
    }
  }
}
