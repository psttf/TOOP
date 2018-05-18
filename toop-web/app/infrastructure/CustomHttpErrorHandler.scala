package infrastructure

import com.rollbar.notifier.Rollbar
import javax.inject.Singleton
import play.api.UsefulException
import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.RequestHeader

@Singleton
class CustomHttpErrorHandler(rollbar: Option[Rollbar])
    extends DefaultHttpErrorHandler {

  override protected def logServerError(
    request: RequestHeader,
    usefulException: UsefulException
  ): Unit =
    rollbar.fold(super.logServerError(request, usefulException))(
      _ log usefulException
    )
}
