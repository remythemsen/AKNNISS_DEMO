package main

import java.io.{File, FileOutputStream, ObjectOutputStream}

import IO.Parser
import LSH.hashFunctions._
import LSH.structures.{HashTable, LSHStructure}
import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by remeeh on 9/26/16.
  */
object Build {
  def main(args:Array[String]) = {
    val parser = new scopt.OptionParser[ConfigBuild]("build") {
      head("AKNNISS Build", "0.x")

      opt[File]('d', "data").required().valueName("<file>").
        action( (x, c) => c.copy(data = x) ).
        text("data to generate LSH Structure from")

      opt[String]('o', "outdir").required().valueName("<path>").
        action( (x, c) => c.copy(outDir = x) ).
        text("dir to store generated LSHStructure")

      opt[Int]('k', "functions").action( (x, c) =>
        c.copy(functions = x) ).text("Number of Hashfunctions")

      opt[Int]('L', "tables").action( (x, c) =>
        c.copy(tables = x) ).text("Number of Hashtables\n")

      opt[String]('h', "hashfunction").action( (x, c) =>
        c.copy(hashFunction = x) ).text("Hashfunction to use\n")

      help("help").text("prints this usage text\n\n")

      note("Approximate K-Nearest Neighbor Image Similarity Search\nCreated by Roxana, Remy and Chris, Fall 2016")

    }

    parser.parse(args, ConfigBuild()) match {
      case Some(config) =>
        // TODO Find new , more random seed
        val seed:Long = System.currentTimeMillis()
        val rnd:Random = new Random(seed)

        val system = ActorSystem("LSHStructureBuilder")
        // Building the structure
        val lshStructure = new LSHStructure(
          Await.result(Future.sequence {
            for {
              i <- 0 until config.tables
              parser <- List(new Parser(new File(config.data.getAbsolutePath)))
              table <- {
                implicit val timeout = Timeout(5.hours)
                val c = system.actorOf(Props(new TableBuilder()))
                val r = c ? BuildTable(() => new Hyperplane(config.functions, () => new Random(rnd.nextLong())), parser, 10000000)
                List(r.asInstanceOf[Future[HashTable]])
              }
            } yield table
            }, Timeout(5.hours).duration))

        system.terminate()
        // Save LSHStructure to file.
        val dir:String = config.outDir.concat("/")
          // constructing filename
          .concat(config.hashFunction)
          .concat("_")
          .concat(config.functions.toString)
          .concat("_")
          .concat(config.tables.toString)
          .concat(".lshstructure")

        val fis = new FileOutputStream(dir.toString)
        val oos = new ObjectOutputStream(fis)
        oos.writeObject(lshStructure)
        oos.close


      case None =>
      // arguments are bad, error message will have been displayed
        println("Invalid Arguments")
    }
  }
}
class TableBuilder extends Actor {
  def receive: Receive = {
    case BuildTable(hf, parser, timeOut) => {
      val t = new HashTable(hf)
      for (j <- 0 until parser.size) {
        println("number:"+j)
        t += parser.next
      }
      sender ! t
    }
  }
}
case class BuildTable(hf:() => HashFunction, parser:Parser, timeOut:Long)
case class ConfigBuild(data: File = new File("."), outDir: String = ".", functions:Int = 12, tables:Int = 4, hashFunction:String = "Hyperplane")


