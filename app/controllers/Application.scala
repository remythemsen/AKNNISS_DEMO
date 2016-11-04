package controllers

import play.api._
import play.api.mvc._
import model._

class Application extends Controller {

  // Initial Index Page
  def index = Action {
    val initQs = Query.lshs.hashTables(0).table.iterator.take(5).map(x => x._2)
    Ok(views.html.index((("000000", Vector()), initQs.next())))
  }

  // Query Request
  def query(pictureId:String) = Action {
    val res = Query.getResults(pictureId, 50.0, 50)
    Ok(views.html.index(res._1, res._2.toList))
  }


}