package blueeyes.core.service.engines.servlet

import javax.servlet.Servlet
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder, ServletContextHandler}
import java.io.File

class JettyServer(servlet: Servlet) {
  private val configFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)

  private var server: Option[Server] = None;

  def start(port: Int){
    configFile.createNewFile();

    val server = new Server(port);
    server.setHandler(servletHandler);
    server.start();
    this.server = Some(server)
  }

  def stop(){
    configFile.delete()

    server.foreach(_.stop())
  }

  private def servletHandler = {
    val servletHandler = new ServletContextHandler(ServletContextHandler.SESSIONS);
    val handler        = new ServletHandler();
    handler.setStartWithUnavailable(false)
    servletHandler.setServletHandler(handler)
    servletHandler.setContextPath("/");

    val holder = new ServletHolder(servlet)
    holder.setInitParameter("configFile", configFile.getAbsolutePath)
    servletHandler.addServlet(holder, "/*");

    servletHandler;
  }
}