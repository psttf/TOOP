package loaders

import com.softwaremill.macwire._
import controllers.AssetsComponents
import infrastructure.{CustomHttpErrorHandler, RollbarComponents}
import org.webjars.play.{RequireJS, WebJarComponents}
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http.HttpErrorHandler
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.filters.hosts.AllowedHostsComponents
import router.Routes
import views.html.index

import scala.concurrent.ExecutionContext

class ToopSigmaApplicationLoader extends ApplicationLoader {
  override def load(context: ApplicationLoader.Context): Application =
    new ToopSigmaComponents(context).application
}

class ToopSigmaComponents(context: Context)
    extends BuiltInComponentsFromContext(context)
    with AssetsComponents
    with AllowedHostsComponents
    with WebJarComponents
    with RollbarComponents
    with ToopSigmaModule {

  override implicit def ec: ExecutionContext = actorSystem.dispatcher

  override def config: Configuration = configuration

  override lazy val httpErrorHandler: HttpErrorHandler =
    new CustomHttpErrorHandler(rollbar)

  lazy val router: Router = {
    val prefix = "/"
    val requireJS: RequireJS = wire[RequireJS]
    val webjarRoutes: webjars.Routes = wire[webjars.Routes]
    wire[Routes]
  }

  override def httpFilters: Seq[EssentialFilter] = Seq(allowedHostsFilter)

  val main: views.html.main = wire[views.html.main]
  override def indexTemplate: index = wire[views.html.index]

  // set up logger
  LoggerConfigurator(context.environment.classLoader).foreach {
    _.configure(context.environment, context.initialConfiguration, Map.empty)
  }

}
