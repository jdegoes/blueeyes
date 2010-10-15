package blueeyes.core.service

import HttpMethods._

trait RestHierarchyBuilder[T] {
  import scala.collection.mutable.{Stack}
  
  private val pathStack: Stack[RestPath] = new Stack[RestPath].push(RootPath);
  private var _hierarchy: List[(RestPath, HttpMethod, RestPathHandler[T])] = Nil
  
  def hierarchy = _hierarchy
  
  def path(path: RestPath)(f: => Unit): Unit = {
    pathStack.push(path)
    
    try { f } finally { pathStack.pop(path) }
  }
  
  def get(handler: RestPathHandler[T]) = custom(GET, handler)
  
  def put(handler: RestPathHandler[T]) = custom(PUT, handler)
  
  def post(handler: RestPathHandler[T]) = custom(POST, handler)
  
  def delete(handler: RestPathHandler[T]) = custom(DELETE, handler)
  
  def options(handler: RestPathHandler[T]) = custom(OPTIONS, handler)
  
  def head(handler: RestPathHandler[T]) = custom(HEAD, handler)
  
  def connect(handler: RestPathHandler[T]) = custom(CONNECT, handler)
  
  def trace(handler: RestPathHandler[T]) = custom(TRACE, handler)
  
  def custom(method: HttpMethod, handler: RestPathHandler[T]) = {
    _hierarchy = (currentPath, method, handler) :: hierarchy
  }
  
  private def currentPath = pathStack.foldRight(RestPath.Root) { (path, element) => path + element }
}