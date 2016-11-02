package controllers

import play.api._
import play.api.mvc._
import model._

class Application extends Controller {

  def index = Action {
    val res = Query.get()
    Ok(views.html.index(res))
  }


}