package controllers

import play.api.mvc._
import model._

class Application extends Controller {

  // Initial Index Page
  def index = Action {
    val initQs = Query.lshs.lookupMap.take(30).toList
    Ok(views.html.index(initQs))
  }

  // Query Request
  def query(pictureId:String) = Action {
    val res = Query.getResults(pictureId, 99.0)
    Ok(views.html.query(res._2, res._1, res._2.length))
  }

}