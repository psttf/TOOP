import controllers.{ApplicationController, AssetsFinder}
import play.api.Configuration
import play.api.mvc.ControllerComponents

import scala.concurrent.ExecutionContext

trait ToopSigmaModule {
  import com.softwaremill.macwire._

  lazy val applicationController: ApplicationController =
    wire[ApplicationController]

  implicit def ec: ExecutionContext
  implicit def assetsFinder: AssetsFinder

  def config: Configuration
  def controllerComponents: ControllerComponents
  def indexTemplate: views.html.index
}
