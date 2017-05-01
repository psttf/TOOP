package controllers

import javax.inject.{Singleton, Inject}

import expressions.{Parser, Semantic}
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

@Singleton
class Application @Inject() (implicit webJarAssets: WebJarAssets) extends Controller {

  val evalForm = Form(
    "code" -> text
  )

  def index = Action {
    Ok(views.html.index(evalForm fill views.txt.code().toString, None))
  }

  def eval = Action { implicit request =>
    val form = evalForm.bindFromRequest
    form("code").value.map({ code =>
      val result = Parser parse code map Semantic.eval
      println(s"result = $result")
      Ok(views.html.index(form, Some(result)))
    }) getOrElse PreconditionFailed(views.html.index(form, None))
  }

}
