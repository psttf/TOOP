package infrastructure

import com.rollbar.notifier.Rollbar
import play.api.Configuration
import rollbar.RollbarConfigProvider

trait RollbarComponents {

  def configuration: Configuration

  lazy val rollbar: Option[Rollbar] =
    RollbarConfigProvider(configuration).rollbarConfig.map(new Rollbar(_))

}
