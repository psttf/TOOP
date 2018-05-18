package rollbar

import com.rollbar.notifier.config.{Config, ConfigBuilder}
import play.api.Configuration

case class RollbarConfigProvider(config: Configuration) {

  lazy val rollbarConfig: Option[Config] =
    for {
      accessToken <- config.getOptional[String]("rollbar.accessToken")
    } yield ConfigBuilder.withAccessToken(accessToken)
      .environment(config.getOptional[String]("rollbar.environment") getOrElse "unknown")
      .language("Scala")
      .framework("Play")
      .build()

}
