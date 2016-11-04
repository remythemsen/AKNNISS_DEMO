package controllers

import play.api._
import play.api.mvc._
import model._

class Application extends Controller {

  // Initial Index Page
  def index = Action {
    val initQs = Query.lshs.hashTables(0).table.iterator.take(25).map(x => ((x._2.map(z => (z, 0.0)))))
    Ok(views.html.index(initQs.next, "unknown"))
  }

  // Query Request
  def query(pictureId:String) = Action {

    val tres = time {
      Query.getResults(pictureId, 2.0, 30)
    }
    Ok(views.html.index(tres._1, tres._2))
  }

  def time[R](block: => R) = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    (result, ((t1 - t0) + "ms"))
  }

}