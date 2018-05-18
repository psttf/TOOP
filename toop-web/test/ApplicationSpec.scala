
import org.specs2.mutable._
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._
import play.api.test.Helpers._

class ApplicationSpec extends Specification {

  val application: Application = GuiceApplicationBuilder().build()

    "Routes" should {

      "send 404 on a bad request" in {
        val Some(result) = route(application, FakeRequest(GET, "/wrongEndpoint"))
        status(result) must equalTo(NOT_FOUND)
      }

      "send 200 on a good request" in {
        val Some(result) = route(application, FakeRequest(GET, "/"))
        status(result) must equalTo(OK)
      }

      "ApplicationController" should {

        "render the index page" in {
          val controller = application.injector.instanceOf[_root_.controllers.Application]
          val result = controller.index()(FakeRequest(GET, "/"))
          status(result) must equalTo(OK)
          contentType(result) must beSome("text/html")
          contentAsString(result) must contain("Sigma Calculus Interpreter")
        }
      }

  }
}
