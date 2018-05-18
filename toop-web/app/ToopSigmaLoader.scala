import controllers.AssetsComponents
import com.softwaremill.macwire._
import infrastructure.{CustomHttpErrorHandler, RollbarComponents}
import play.api.ApplicationLoader.Context
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api._
import play.api.http.HttpErrorHandler
import play.filters.hosts.AllowedHostsComponents
import router.Routes

import scala.concurrent.ExecutionContext

class ToopSigmaLoader extends ApplicationLoader {
  override def load(context: ApplicationLoader.Context): Application =
    new ToopSigmaComponents(context).application
}

class ToopSigmaComponents(context: Context)
    extends BuiltInComponentsFromContext(context)
    with AssetsComponents
    with AllowedHostsComponents
    with RollbarComponents
    with ToopSigmaModule {

  override implicit def ec: ExecutionContext = actorSystem.dispatcher

  override def config: Configuration = configuration

  override lazy val httpErrorHandler: HttpErrorHandler = new CustomHttpErrorHandler(rollbar)

  lazy val router: Router = {
    val prefix = "/"
    wire[Routes]
  }

  override def httpFilters: Seq[EssentialFilter] = Seq(allowedHostsFilter)

  // set up logger
  LoggerConfigurator(context.environment.classLoader).foreach {
    _.configure(context.environment, context.initialConfiguration, Map.empty)
  }

}