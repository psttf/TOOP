package controllers

import java.util.concurrent.Executors

import data.json._
import expressions.{Parser, Semantic, Term, SemanticState}
import io.circe.syntax._
import javax.inject.{Inject, Singleton}
import models.{EvalFailure, EvalSuccess}
import monix.execution.FutureUtils.extensions._
import monix.execution.Scheduler
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.circe.Circe
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Try}

@Singleton
class Application @Inject()(
  cc: ControllerComponents,
  config: Configuration,
  indexTemplate: views.html.index
)(implicit assetsFinder: AssetsFinder)
    extends AbstractController(cc) with Circe {

  val ec: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  implicit val scheduler: Scheduler = Scheduler(ec)

  val evalForm = Form(
    "code" -> text
  )

  def index = Action {
    Ok(indexTemplate(evalForm.fill(views.txt.code().toString), None))
  }

  def eval: Action[AnyContent] = Action.async { implicit request =>
    val form = evalForm.bindFromRequest
    evalCode(form)
      .map(
        result =>
          Status(
            result.fold(_ => UNPROCESSABLE_ENTITY, _ => OK)
          )(
            indexTemplate(form, Some(result))
          )
      )
  }

  def evalJson: Action[AnyContent] = Action.async { implicit request =>
    evalCode(evalForm.bindFromRequest)
      .map(
        _.fold(
          error =>
            Status(UNPROCESSABLE_ENTITY)(EvalFailure(error.toString).asJson),
          term =>
            Status(OK)(EvalSuccess(term.toString).asJson)
        )
      )
  }

  private def evalCode(form: Form[String]): Future[Try[Term]] =
    form("code").value
      .map (
        code =>
          Future { Parser.parse(code).map(Semantic.eval(_) match {
            case SemanticState(Right(term), _) => term
            case SemanticState(Left(err), _) => throw err
          })}
            .timeoutTo(
              config.get[Int]("parserFuture.timeoutInSeconds").seconds,
              Future.failed(new TimeoutException)
            )
            .recover {
              case err: TimeoutException => Failure(err)
            }
      )
      .getOrElse(Future.successful(
        Failure(new IllegalArgumentException("code is required"))
      ))

}
