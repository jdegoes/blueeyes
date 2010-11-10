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

/**
 * An http service, which responds to http requests with http responses. 
 * Services are typed in whatever type is required by the server engine.
 * For example, some server engines might only deal with strings.
 */
trait HttpService2[T] {
  def name: String
  
  def version: String
  
  def majorVersion: Int = version.split(".").drop(0).headOption.map(_.toInt).getOrElse(0)
  
  def minorVersion: Int = version.split(".").drop(1).headOption.map(_.toInt).getOrElse(0)
  
  def descriptorFactory: HttpServiceContext => HttpServiceDescriptor[T, _]
  
  def ioClass: Class[T]
  
  override def toString = name + "." + majorVersion
}