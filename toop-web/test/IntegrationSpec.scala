import org.scalatestplus.play._
import utils.ApplicationTestFactory

class IntegrationSpec
    extends PlaySpec
    with BaseOneServerPerTest
    with OneBrowserPerTest
    with ApplicationTestFactory
    with HtmlUnitFactory {

  "Application" should {

    "work from within a browser" in {
      go to s"http://localhost:$port"
      pageSource must include("Write some code")
    }

  }

}
