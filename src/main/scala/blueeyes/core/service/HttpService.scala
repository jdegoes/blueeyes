package blueeyes.core.service

/**
 * An http service - the fundamental concept in Blue Eyes.
 */
trait HttpService[S] extends RestHierarchy[S] {
  /** The short name of the service, e.g. "email"
   */
  def name: String
  
  /** The version of the service, e.g. "32" 
   */
  def version: Int
  
  /** The root path of the service, which defaults to "/"
   */
  def rootPath: String = "/"
}

import blueeyes.core.http._
import blueeyes.util.Future
import net.lag.configgy.{Config, ConfigMap}
import net.lag.logging.Logger

/**
 * An http service, which responds to http requests with http responses. 
 * Services are typed in whatever type is required by the server implementation.
 * For example, some server implementations might only deal with strings.
 */
trait HttpService2[Base] extends PartialFunction[HttpRequest[Base], Future[HttpResponse[Base]]] {
  private type Handler = PartialFunction[HttpRequest[Base], Future[HttpResponse[Base]]]
  
  def name:     String
  def config:   ConfigMap
  def log:      Logger
  def version:  String
  
  def majorVersion: Int = version.split(".").drop(0).headOption.map(_.toInt).getOrElse(0)
  def minorVersion: Int = version.split(".").drop(1).headOption.map(_.toInt).getOrElse(0)
  
  def startHooks:         List[() => Future[_]] = Nil
  def stopHooks:          List[() => Future[_]] = Nil
  def notFoundHandler:    Option[HttpNotFoundHandler[_, _, Base]] = None
  def pathMethodHandlers: List[HttpPathMethodHandler[_, _, Base]] = Nil
  
  def status: RunningStatus = _status
  
  def start: Future[Unit] = {
    _status = RunningStatus.Starting
    
    runAllHooks(startHooks, _status = RunningStatus.Started, _status = RunningStatus.Errored)
  }
  
  def stop: Future[Unit] = {
    _status = RunningStatus.Stopping
    
    runAllHooks(stopHooks, _status = RunningStatus.Stopped, _status = RunningStatus.Errored)
  }
  
  def install(container: HttpServicesContainer[Base]): Unit
  
  def allHandlers: List[Handler] = {
    val collector: PartialFunction[AnyRef, Handler] = { case e: Handler => e }
    
    pathMethodHandlers.collect(collector) ++ notFoundHandler.toList.collect(collector)
  }
  
  def isDefinedAt(request: HttpRequest[Base]) = masterHandler.isDefinedAt(request)
  
  def apply(request: HttpRequest[Base]) = masterHandler.apply(request)
  
  override def toString = name + "-" + version
  
  private var _status: RunningStatus = RunningStatus.Stopped
  
  private lazy val masterHandler: PartialFunction[HttpRequest[Base], Future[HttpResponse[Base]]] = {
    allHandlers.foldLeft[Handler](HttpNoHandler()) { (composite, cur) => composite.orElse(cur: Handler) }
  }
  
  private def runAllHooks(hooks: List[() => Future[_]], onDone: => Unit, onError: => Unit): Future[Unit] = {
    var result = new Future[Unit]
    
    var hookCount: Int = hooks.length
    
    hooks.foreach {
      f => f().deliverTo { _ =>
        hookCount = hookCount - 1
        
        if (hookCount == 0) {
          result.deliver(Unit)
          
          onDone
        }
      }.ifCanceled { e =>
        onError
      }
    }
    
    result
  }
}