package blueeyes.core.service.engines

import scala.collection.JavaConversions._
import org.jboss.netty.channel._
import org.jboss.netty.handler.codec.http.HttpHeaders.Names
import org.jboss.netty.handler.codec.http.HttpHeaders.Names._
import org.jboss.netty.handler.codec.http.HttpHeaders._
import org.jboss.netty.buffer.{ChannelBuffer}
import blueeyes.util.RichThrowableImplicits._
import blueeyes.core.data.Bijection
import blueeyes.core.service._
import blueeyes.util.Future
import blueeyes.util.Future._
import blueeyes.core.http._
import net.lag.logging.Logger
import org.jboss.netty.handler.timeout.TimeoutException
import org.jboss.netty.handler.codec.http.{HttpRequest => NettyHttpRequest}

private[engines] class NettyRequestHandler[T] (requestHandler: HttpRequestHandler[T], log: Logger)(implicit contentBijection: Bijection[ChannelBuffer, T]) extends SimpleChannelUpstreamHandler with NettyConverters{
  private val futures = new FuturesList[HttpResponse[T]]()

  override def messageReceived(ctx: ChannelHandlerContext, event: MessageEvent) {
    try {
      val nettyRequest   = event.getMessage().asInstanceOf[NettyHttpRequest]
      val request        = fromNettyRequest(nettyRequest, event.getRemoteAddress)
      val requestFuture  = handleRequest(request)

      futures + requestFuture

      requestFuture.deliverTo(response => {
        futures - requestFuture
        writeResponse(event) (response)
      })

      requestFuture.ifCanceled{why =>
        futures - requestFuture
        why.foreach{t => if (!t.isInstanceOf[TimeoutException]) {
            writeResponse(event) (toResponse(t))
          }
        }
        why.orElse{
          event.getChannel.close
          None
        }
      }
    }
    catch {
      case e: Throwable => log.error(e, "Error while request handling. Request=" + event); writeResponse(event) (toResponse(e))
    }
  }

  override def channelDisconnected(ctx: ChannelHandlerContext, e: ChannelStateEvent) = {
    super.channelDisconnected(ctx, e)
    futures.cancel
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    val error    = e.getCause

    log.error(error, "ExceptionCaught")
    e.getChannel().close
  }

  private def handleRequest(request: HttpRequest[T]): Future[HttpResponse[T]] = {
    if (requestHandler.isDefinedAt(request)) requestHandler(request)
    else new Future[HttpResponse[T]]().deliver(HttpResponse(HttpStatus(HttpStatusCodes.NotFound)))
  }

  private def toResponse(th: Throwable) = th match{
    case e: HttpException => HttpResponse[T](HttpStatus(e.failure, e.reason))
    case _ => {
      val reason = th.fullStackTrace
      HttpResponse[T](HttpStatus(HttpStatusCodes.InternalServerError, if (reason.length > 3500) reason.substring(0, 3500) else reason))
    }
  }

  private def writeResponse(e: MessageEvent)(response: HttpResponse[T]){
    val request       = e.getMessage().asInstanceOf[NettyHttpRequest]
    val nettyResponse = toNettyResponse(response)
    val keepAlive     = isKeepAlive(request)

    if (keepAlive) nettyResponse.setHeader(Names.CONTENT_LENGTH, nettyResponse.getContent().readableBytes())

    val future = e.getChannel().write(nettyResponse)

    if (!keepAlive) future.addListener(ChannelFutureListener.CLOSE)
  }
}

private[engines] class FuturesList[T]{
  private val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
         
  private var _futures = List[Future[T]]()
  private var cancelError: Option[TimeoutException] = None

  def +(future: Future[T]){
    writeLock {
      cancelError.foreach(future.cancel(_))

      if (!future.isCanceled){
        _futures = future :: _futures 
      }
    }
  }
  def -(future: Future[T]){
    writeLock {
      _futures = _futures.filterNot(_ == future)
    }
  }

  def cancel{
    writeLock {
      cancelError = Some(new TimeoutException("Connection closed."))

      _futures.foreach(_.cancel(cancelError))

      _futures = Nil
    }
  }

  def futures = readLock(() => {_futures})

  private def readLock[S](f: => S): S = {
    lock.readLock.lock()
    try {
      f
    }
    finally {
      lock.readLock.unlock()
    }
  }

  private def writeLock[S](f: => S): S = {
    lock.writeLock.lock()
    try {
      f
    }
    finally {
      lock.writeLock.unlock()
    }
  }
}
