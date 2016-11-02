package controllers

import play.api._
import play.api.mvc._
import model._

class Application extends Controller {

  // Initial Index Page
  def index = Action {
    val initQs = Query.lshs.hashTables(0).table.valuesIterator.take(10)
    Ok(views.html.index(initQs.next()))
  }

  // Query Request
  def query(pictureId:String) = Action {
    val res = Query.getResults(pictureId, 50.0, 50)
    Ok(views.html.index(res.toList))
  }


}