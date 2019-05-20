import org.scalatestplus.play.{BaseOneAppPerTest, PlaySpec}
import play.api.test._
import play.api.test.Helpers._
import utils.ApplicationTestFactory

class ApplicationSpec
    extends PlaySpec
    with BaseOneAppPerTest
    with ApplicationTestFactory {

  "Routes" should {

    "send 404 on a bad request" in {
      val Some(result) = route(app, FakeRequest(GET, "/wrongEndpoint"))
      status(result) mustBe NOT_FOUND
    }

    "send 200 on a good request" in {
      val Some(result) = route(app, FakeRequest(GET, "/"))
      status(result) mustBe OK
    }

  }

  "ApplicationController" should {

    "render the index page" in {
      val result = route(app, FakeRequest(GET, "/")).get
      status(result) mustBe OK
      contentType(result) mustBe Some("text/html")
      contentAsString(result) must include("Sigma Calculus Interpreter")
    }
  }

}
