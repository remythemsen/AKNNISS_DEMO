package main

import java.io._

import breeze.linalg.DenseMatrix
import preProcessing.DimensionalityReducer

import scala.io.Source

object Preprocess {
  def main(args:Array[String]) = {
    val parser = new scopt.OptionParser[ConfigPreprocessing]("Dimensional reducer") {
      head("AKNNISS Preprocessor Dimensionality Reducer", "0.x")

      opt[File]('d', "data").required().valueName("<file>").
        action( (x, c) => c.copy(data = x) ).
        text("File to be preprocessed")

      opt[String]('o', "outdir").required().valueName("<path>").
        action( (x, c) => c.copy(outDir = x) ).
        text("dir to store preprocessed file")

      opt[Int]('v', "vlength").action( (x, c) =>
        c.copy(vlength = x) ).text("number of components in each vector\n")

      help("help").text("prints this usage text\n\n")

      note("Approximate K-Nearest Neighbor Image Similarity Search\nCreated by Roxana, Remy and Chris, Fall 2016")

    }

    // parser.parse returns Option[C]
    parser.parse(args, ConfigPreprocessing()) match {
      case Some(config) =>
        val A:DenseMatrix[Float]= DimensionalityReducer.getRandMatrix(20000000,4096);

        // Reducing and saving
        val input = new BufferedInputStream( new FileInputStream( config.data ) )

        // Save LSHStructure to file.
        val dir:String = config.outDir.concat("/")
          // constructing filename
          .concat(config.data.getName.substring(0,config.data.getName.length-5))
          .concat("-reduced.data")

        val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dir.toString)))
        val input2 = Source.fromFile(config.data.getAbsoluteFile).getLines()

        var j = 0.0
        val size = Source.fromFile(config.data.getAbsolutePath).getLines().length/2
        println(size)

        // IDEA: Can we have one thread reading, and one writing ?

        var sb = new StringBuilder
        while(input2.hasNext) {
          val l = input2.next
          if(l.charAt(0).equals('#')) {
            // Then get the ID
            sb.append(l.substring(49)+"\n")
          } else {
            // Get the vector
            val v = DimensionalityReducer.getNewVector(l.split(" ").map(x => x.toFloat), A)
            for(i <- v) {
              sb.append(i + " ")
            }
            sb.append("\n")

            // Write resulting set
            output.write(sb.toString())

            j+=1.0
            println(((j / size) * 100).toString.substring(0, 3)+"%")
            sb = new StringBuilder
          }
        }
        println("Finished with "+size+" tuples")

      case None =>
        // arguments are bad, error message will have been displayed
        println("Invalid Arguments")
    }
  }
}
case class ConfigPreprocessing(data: File = new File("."), outDir: String = ".", vlength:Int = 4096)
