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

class ApplicationController(
  cc: ControllerComponents,
  config: Configuration,
  indexTemplate: views.html.index
)(implicit assetsFinder: AssetsFinder, ec: ExecutionContext)
    extends AbstractController(cc) {

  implicit val scheduler: Scheduler = Scheduler(ec)

  val evalForm = Form(
    "code" -> text
  )

  def index = Action {
    Ok(indexTemplate(evalForm.fill(views.txt.code().toString), None))
  }

  def eval: Action[AnyContent] = Action.async { implicit request =>
    val form = evalForm.bindFromRequest
    form("code").value
      .map({ code =>
        val result = Future { Parser.parse(code).map(Semantic.eval) }
          .timeoutTo(
            config.get[Int]("parserFuture.timeoutInSeconds").seconds,
            Future.failed(
              new TimeoutException(
                "Ваше вычисление заняло слишком много времени и было остановлено."
              )
            )
          )
        result
          .map(parsedTerm => Ok(indexTemplate(form, Some(parsedTerm))))
          .recoverWith {
            case err: TimeoutException =>
              Future { Ok(indexTemplate.apply(form, Some(Failure(err)))) }
          }
      })
      .getOrElse(Future { PreconditionFailed(indexTemplate.apply(form, None)) })
  }

}
