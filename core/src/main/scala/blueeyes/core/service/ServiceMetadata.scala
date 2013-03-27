package blueeyes
package core.service

import blueeyes.core.http._
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._
import blueeyes.core.http.HttpHeaders.{`Content-Type`}

import scalaz.Scalaz._
import scalaz.{Validation, Failure, Semigroup}
import scalaz.syntax.semigroup._

sealed trait NotServed {
  def or[A](result: => Validation[NotServed, A]): Validation[NotServed, A]
}

case class DispatchError(exception: HttpException) extends NotServed {
  override def or[A](result: => Validation[NotServed, A]) = this.failure[A]
}

object DispatchError {
  def apply(failure: HttpFailure, reason: String): DispatchError = DispatchError(HttpException(failure, reason))
  def apply(t: (HttpFailure, String)): DispatchError = DispatchError(HttpException(t._1, t._2))
}

case class Inapplicable private[service] (services: AnyService*) extends NotServed {
  override def or[A](result: => Validation[NotServed, A]) = result match {
    case Failure(Inapplicable(others @ _*)) => Inapplicable(services ++ others: _*).failure[A]
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

case class CookieMetadata(ident: Symbol, default: Option[String]) extends Metadata(6) {
  override def compare(other: Metadata) = other match {
    case CookieMetadata(i, _) => ident.toString.compare(i.toString)
    case _ => super.compare(other)
  }
}

case class TitleMetadata        (title: String) extends Metadata(7)
case class DescriptionMetadata  (description: String) extends Metadata(8)
case class DataSizeMetadata     (dataSize: DataSize)  extends Metadata(9)
case class EncodingMetadata     (encodings: Encoding*)  extends Metadata(10)
case class AboutMetadata        (metadata: Metadata, about: Metadata) extends Metadata(11)
case class AndMetadata          (metadata: Metadata*) extends Metadata(12)
case class OrMetadata           (metadata: Metadata*) extends Metadata(13)
case object NoMetadata extends Metadata(14)

object Metadata {
  def pathPattern(pattern: RestPathPattern): Metadata = PathPatternMetadata(pattern)
  def httpMethod(method: HttpMethod): Metadata = HttpMethodMetadata(method)
  def requestHeaderField[T <: HttpHeader](header: HttpHeaderField[T], default: Option[T] = None): Metadata = RequestHeaderMetadata(Left(header), default)
  def requestHeader[T <: HttpHeader](header: T, default: Option[T] = None): Metadata = RequestHeaderMetadata(Right(header), default)
  def responseHeaderField[T <: HttpHeader](header: HttpHeaderField[T], default: Option[T] = None): Metadata = ResponseHeaderMetadata(Left(header), default)
  def responseHeader[T <: HttpHeader](header: T, default: Option[T] = None): Metadata = ResponseHeaderMetadata(Right(header), default)
  def parameter(param: Symbol, default: Option[String] = None): Metadata = ParameterMetadata(param, default)
  def cookie(ident: Symbol, default: Option[String] = None): Metadata = CookieMetadata(ident, default)
  def title(title: String): Metadata = TitleMetadata(title)
  def description(desc: String): Metadata = DescriptionMetadata(desc)
  def dataSize(dataSize: DataSize): Metadata = DataSizeMetadata(dataSize)
  def encodings(enc: Encoding*) = EncodingMetadata(enc: _*)
  def about(m1: Metadata, m2: Metadata) = AboutMetadata(m1, m2)
  def and(ms: Metadata*) = AndMetadata(ms: _*)
  def or(ms: Metadata*) = OrMetadata(ms: _*)

  implicit def opt2M(opt: Option[Metadata]): Metadata = opt getOrElse NoMetadata

  implicit object MetadataSemigroup extends Semigroup[Metadata] {
    private def paths(m: List[Metadata]) = {
      val merge = m.foldLeft((List[Metadata](), None: Option[Metadata], None: Option[DescriptionMetadata], None: Option[PathPatternMetadata])){
        case ((ms, Some(d1 @ DescriptionMetadata(_)), d, p), p1 @ PathPatternMetadata(_)) =>
          val desc = d.map(v => DescriptionMetadata(v.description + d1.description)).getOrElse(d1)
          val path = p.map(v => PathPatternMetadata(v.pattern ~ p1.pattern)).getOrElse(p1)
          (ms, None, Some(desc), Some(path))
        case ((ms, last, d, p), p1 @ PathPatternMetadata(_)) =>
          val path = p.map(v => PathPatternMetadata(v.pattern ~ p1.pattern)).getOrElse(p1)
          (ms ::: last.toList, None, d, Some(path))
        case ((ms, last, d, p), m) => (ms ::: last.toList, Some(m), d, p)
      }
      merge._3.toList ::: merge._4.toList ::: merge._1 ::: merge._2.toList
    }

    private def mergeDesc(m: Metadata*) = {
      val merge = m.tail.foldLeft((List[Metadata](), m.head)){
        case ((ms, DescriptionMetadata(desc1)), DescriptionMetadata(desc2)) => (ms, DescriptionMetadata(desc1 + desc2))
        case ((ms, last), m) => (ms ::: List(last), m)
      }
      merge._1 ::: List(merge._2)
    }

    def append(a: Metadata, b: => Metadata) = {
      val ab: Seq[Metadata] = ((a, b) match {
        case (AndMetadata(ams @ _*), AndMetadata(bms @ _*)) => ams ++ bms
        case (AndMetadata(ams @ _*), other) => ams :+ other
        case (other, AndMetadata(bms @ _*)) => other +: bms
        case (NoMetadata, other) => Seq(other)
        case (other, NoMetadata) => Seq(other)
        case _ => Seq(a, b)
      }) filter {
        _ != NoMetadata
      } 

      if (ab.isEmpty) NoMetadata else AndMetadata(paths(mergeDesc(ab: _*)): _*)
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
        //TODO: is OrMetadata really only used for parameters?
        Append(Seq(Nest(ValueCaption("Parameter Type", "Parameter", "Description")), Break) ++
        metadata.map(m => Nest(formatMetadata(m)): Printable[String]).toList.intersperse(Append(Break, Break)): _*)

      case AboutMetadata(metadata, about) => 
        formatMetadata(metadata) ~ Break ~ Nest(formatMetadata(about))

      case AndMetadata(metadata @ _*) => metadata match {
        case Seq(DescriptionMetadata(desc), metadata) => formatMetadata(metadata) match {
          case Value(valueType, value, valueDesc) => Value(valueType, value, Some(desc + valueDesc.getOrElse("")))
          case p => Append(Description(desc), Break, p)
        }
        case _ =>
          val grouped = metadata.reverse.foldLeft(List[Metadata]()) {
            case (x :: xs, d: DescriptionMetadata) => AndMetadata(d, x) :: xs
            case (xs, m) => m :: xs
          }
          val sorted = grouped.sortWith{
            case (AndMetadata(DescriptionMetadata(desc), metadata @ _), m) => metadata.compare(m) < 0
            case (m, AndMetadata(DescriptionMetadata(desc), metadata @ _)) => m.compare(metadata) < 0
            case (m1, m2) => m1.compare(m2) < 0
          }
          Append(sorted.map(formatMetadata).toList.intersperse(Break): _*)
      }

      case _ => Empty
    }
  }

  def serviceMetadata(service: AnyService): Metadata = {
    def metadata(m: Metadata, service: AnyService): Metadata = service match {
      case OrService(services @ _*)  =>
        val ms = services.map(metadata(m, _))
        Some(OrMetadata(ms.foldLeft(Seq[Metadata]()){
          case (l, OrMetadata(ms @ _*)) => l ++ ms
          case (l, m) => l :+ m
        }: _*))
      case s: DelegatingService[_, _, _, _] => metadata(m |+| nePath(s.metadata), s.delegate)
      case s => m |+| nePath(s.metadata)
    }

    def nePath(m: Metadata) = m match {
      case PathPatternMetadata(RestPathPatternParsers.EmptyPathPattern) => NoMetadata
      case other => other
    }

    metadata(None, service) 
  }
}

//type ServiceMetadata //ctags help
