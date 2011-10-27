package blueeyes
package core.service

import blueeyes.core.http._
import blueeyes.util.metrics.DataSize
import blueeyes.util.printer._

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

case class TitleMetadata        (title: String) extends Metadata(6)
case class DescriptionMetadata  (description: String) extends Metadata(7)
case class DataSizeMetadata     (dataSize: DataSize)  extends Metadata(8)
case class EncodingMetadata     (encodings: Encoding*)  extends Metadata(9)
case class AndMetadata          (metadata: Metadata*) extends Metadata(10)
case class OrMetadata           (metadata: Metadata*) extends Metadata(11)

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

      val pm = ap ::: bp match{
        case Nil => List[Metadata]()
        case abp => List(abp.tail.foldLeft(abp.head){(m, p) => PathPatternMetadata(m.pattern ~ p.pattern, m.desc ⊹ p.desc)})
      }
      AndMetadata(pm ++ am ++ bm: _*)
    }
  }  

  implicit object StringFormatter extends Formatter[Metadata, String] {
    def format(m: Metadata) = formatMetadata(m)

    def formatMetadata(m: Metadata): Printable[String] = m match {
      case PathPatternMetadata(pattern, desc) => Value("Service path", pattern.toString, desc)
      case HttpMethodMetadata(method)         => Value("HTTP method", method.toString, None)
      case HeaderMetadata(header, _, desc)    => Value("Header", header.fold(_.name, _.header), desc)

      case ParameterMetadata(parameter, default, desc) =>
        Value("Request Parameter", parameter.toString, Some(desc.getOrElse("")
              + default.map("; a default of " + _ + " will be used if no value is specified.").getOrElse(" (required)")))

      case CookieMetadata(parameter, default, desc) =>
        Value("HTTP cookie", parameter.toString, Some(desc.getOrElse("")
              + default.map("; a default of " + _ + " will be used if no value is specified.").getOrElse(" (required)")))

      case EncodingMetadata(encodings @ _*) => Value("Supported encodings", encodings.mkString(", "), None)
      case DescriptionMetadata(desc) => Append(Description(desc), Break)
      case TitleMetadata(title) => Append(Title(title), Break)

      case OrMetadata(metadata @ _*) =>
        Append(Seq(Nest(ValueCaption("Parameter Type", "Parameter", "Description")), Break) ++
        metadata.map(m => Nest(formatMetadata(m)): Printable[String]).toList.intersperse(Append(Break, Break)): _*)

      case AndMetadata(metadata @ _*) => Append(metadata.sorted.map(formatMetadata).toList.intersperse(Break): _*)

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
      case PathPatternMetadata(RestPathPatternParsers.EmptyPathPattern, _) => None
      case _ => m
    }

    metadata(None, service) match {
      case Some(m: OrMetadata) => m
      case Some(m) => OrMetadata(m)
      case None => OrMetadata()
    }
  }
}
