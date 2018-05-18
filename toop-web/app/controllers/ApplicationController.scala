package controllers

import expressions.{Parser, Semantic}
import monix.execution.FutureUtils.extensions._
import monix.execution.Scheduler
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Failure

class ApplicationController (
  cc: ControllerComponents,
  config: Configuration
)(implicit assetsFinder: AssetsFinder, ec: ExecutionContext)
    extends AbstractController(cc) {

  implicit val scheduler: Scheduler = Scheduler(ec)

  val evalForm = Form(
    "code" -> text
  )

  def index = Action {
    Ok(views.html.index(evalForm.fill(views.txt.code().toString), None))
  }

  def eval: Action[AnyContent] = Action.async { implicit request =>
    val form = evalForm.bindFromRequest
    form("code").value
      .map({ code =>
        val result = Future { Parser.parse(code).map(Semantic.eval) }
          .timeoutTo(
            config.get[Int]("parserFuture.timeoutInSeconds").seconds,
            Future.failed(new TimeoutException)
          )
        result
          .map(parsedTerm => Ok(views.html.index(form, Some(parsedTerm))))
          .recoverWith {
            case err: TimeoutException =>
              Future { Ok(views.html.index.apply(form, Some(Failure(err)))) }
          }
      })
      .getOrElse(Future { PreconditionFailed(views.html.index.apply(form, None)) })
  }

}
