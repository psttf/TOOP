package controllers

import javax.inject.{Inject, Singleton}
import expressions.{Parser, Semantic}
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{AbstractController, ControllerComponents}

@Singleton
class Application @Inject()(cc: ControllerComponents, indexTemplate: views.html.index)
                           (implicit assetsFinder: AssetsFinder)
  extends AbstractController(cc) {

  val evalForm = Form(
    "code" -> text
  )

  def index = Action {
    Ok(indexTemplate(evalForm fill views.txt.code().toString, None))
  }

  def eval = Action { implicit request =>
    val form = evalForm.bindFromRequest
    form("code").value.map({ code =>
      val result = Parser parse code map Semantic.eval
      println(s"result = $result")
      Ok(indexTemplate(form, Some(result)))
    }) getOrElse PreconditionFailed(indexTemplate(form, None))
  }

}
