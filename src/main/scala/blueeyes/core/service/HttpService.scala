package blueeyes.core.service

/**
 * An http service, which responds to http requests with http responses. 
 * Services are typed in whatever type is required by the server engine.
 * For example, some server engines might only deal with strings.
 */
trait Service[T] {
  def name: String
  
  def version: ServiceVersion

  def desc: Option[String]

  def descriptorFactory: ServiceContext => ServiceDescriptor[T, _]
  
  def ioClass: Class[T]
  
  override def toString = name + "." + version.majorVersion
}
