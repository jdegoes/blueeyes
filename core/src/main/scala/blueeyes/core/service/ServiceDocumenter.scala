package blueeyes.core.service

import blueeyes.util.printer.{Printer, Formatter}

object ServiceDocumenter {
  def metadata(context: ServiceContext, service: AnyService) = {
    val metadata     = Metadata.serviceMetadata(service)
    val title        = TitleMetadata("REST API Resources | %s (%d.%d.%s)".format(context.serviceName, context.serviceVersion.majorVersion, context.serviceVersion.minorVersion, context.serviceVersion.version))
    val titleAndDesc = title :: context.desc.map(desc => List[Metadata](DescriptionMetadata(desc))).getOrElse(Nil)

    AndMetadata(titleAndDesc ::: List(metadata): _*)
  }

  def printFormatted(context: ServiceContext, service: AnyService)(implicit formatter: Formatter[Metadata, String], printer: Printer[String]) = printer.printFormatted(metadata(context, service))
}
