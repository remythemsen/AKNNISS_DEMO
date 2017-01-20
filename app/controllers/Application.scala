package controllers
import java.io.File

import akka.actor.ActorSystem
import com.google.inject.Inject
import utils.IO.ReducedFileParser
import play.api.mvc._
import model._

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Application @Inject() (system: ActorSystem)  extends Controller {
  println("     ___    __ __ _   ___   ________________")
  println("   /   |  / //_// | / / | / /  _/ ___/ ___/  ")
  println("  / /| | / ,<  /  |/ /  |/ // / \\__ \\\\__ \\   ")
  println(" / ___ |/ /| |/ /|  / /|  // / ___/ /__/ /   ")
  println("/_/  |_/_/ |_/_/ |_/_/ |_/___//____/____/    ")
  println("\nAproximate K-Nearest Neighbor Image Similarity Search")


  val lshs = new LSHS(system)

  println("building lookup table...")
  val lookupTable:HashMap[Int, Array[Float]] = {
    val parser = new ReducedFileParser(new File("data/100k_direct_sample_norm.data"))
    var hm = new HashMap[Int, Array[Float]]
    while(parser.hasNext) {
      hm += parser.next
    }
    hm
  }

  println("lookup table is done..")

  lshs.getStarted

  // Initial Index Page
  def index = Action {
    val initQs:ArrayBuffer[String] = lookupTable.keys.take(30).map(x => addZeroes(x)).to[mutable.ArrayBuffer]
    Ok(views.html.index(initQs))
  }

  // Query Request
  def query(pictureId:Int) = Action {
    val res:(ArrayBuffer[(Int, Float)], Int, Double) = lshs.makeQuery((pictureId, lookupTable(pictureId)))
    Ok(views.html.query((res._1.map(x => (addZeroes(x._1), x._2)),addZeroes(res._2), res._3)))
  }
  def addZeroes(id:Int):String = {
    val zeroC = 10 - id.toString.length
    var zeroes = {
      val sb = new StringBuilder
      for(i <- 0 until zeroC) {
        sb.append("0")
      }
      sb
    }
    zeroes.append(id).toString
  }

}