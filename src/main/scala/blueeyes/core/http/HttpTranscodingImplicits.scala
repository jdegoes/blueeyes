package blueeyes.core.http

import blueeyes.util.Future
import blueeyes.core.data._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.http.HttpHeaderImplicits._

trait HttpTranscodingImplicits {
  implicit def requestToForwardTranscodedRequest[T, S](request: HttpRequest[T])(implicit transcoder: DataTranscoder[T, S]): HttpRequest[S] = {
    request.copy(content = request.content.map(transcoder.transcode.apply), headers = request.headers + `Content-Type`(transcoder.mimeType))
  }
  
  implicit def requestToBackwardTranscodedRequest[T, S](request: HttpRequest[S])(implicit transcoder: DataTranscoder[T, S]): HttpRequest[T] = {
    request.copy(content = request.content.map(transcoder.transcode.unapply), headers = request.headers + `Content-Type`(transcoder.mimeType))
  }
  
  implicit def responseToForwardTranscodedResponse[T, S](response: HttpResponse[T])(implicit transcoder: DataTranscoder[T, S]): HttpResponse[S] = {
    response.copy(content = response.content.map(transcoder.transcode.apply), headers = response.headers + `Content-Type`(transcoder.mimeType))
  }
  
  implicit def responseToBackwardTranscodedResponse[T, S](response: HttpResponse[S])(implicit transcoder: DataTranscoder[T, S]): HttpResponse[T] = {
    response.copy(content = response.content.map(transcoder.transcode.unapply), headers = response.headers + `Content-Type`(transcoder.mimeType))
  }
  
  implicit def futureResponseToForwardTranscodedFutureResponse[T, S](response: Future[HttpResponse[T]])(implicit transcoder: DataTranscoder[T, S]): Future[HttpResponse[S]] = {
    response.map { response =>
      response.copy(content = response.content.map(transcoder.transcode.apply), headers = response.headers + `Content-Type`(transcoder.mimeType))
    }
  }
  
  implicit def futureResponseToBackwardTranscodedFutureResponse[T, S](response: Future[HttpResponse[S]])(implicit transcoder: DataTranscoder[T, S]): Future[HttpResponse[T]] = {
    response.map { response =>
      response.copy(content = response.content.map(transcoder.transcode.unapply), headers = response.headers + `Content-Type`(transcoder.mimeType))
    }
  }
}
object HttpTranscodingImplicits extends HttpTranscodingImplicits