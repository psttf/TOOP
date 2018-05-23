package utils

import loaders.ToopSigmaApplicationLoader
import org.scalatestplus.play.FakeApplicationFactory
import play.api.inject.DefaultApplicationLifecycle
import play.api.{Application, ApplicationLoader, Configuration, Environment}
import play.core.DefaultWebCommands

trait ApplicationTestFactory extends FakeApplicationFactory {

  private class ApplicationBuilder {
    def build(): Application = {
      val env = Environment.simple()
      val context = ApplicationLoader.Context(
        environment = env,
        sourceMapper = None,
        webCommands = new DefaultWebCommands(),
        initialConfiguration = Configuration.load(env),
        lifecycle = new DefaultApplicationLifecycle()
      )
      val loader = new ToopSigmaApplicationLoader()
      loader.load(context)
    }
  }

  def fakeApplication(): Application = new ApplicationBuilder().build()

}
