package preProcessing

import breeze.linalg.DenseMatrix
import org.scalatest._
import scala.util.Random

class DimensionalityReducerSpec extends FlatSpec with Matchers {
  "All outvectors" should "have the same length" in {
    val rnd = new Random(System.currentTimeMillis())
    val A:DenseMatrix[Double]= DimensionalityReducer.getRandMatrix(20000000,4096);

    var prev:Vector[Double] = Vector.empty
    for(i <- 0 until 100) {
      val vIn:Vector[Double] = Vector.fill(4096)(rnd.nextDouble)
      val vOut:Vector[Double] = DimensionalityReducer.getNewVector(vIn,A)
      if(prev != Vector.empty) {
        prev.size should be (vOut.size)
      }
      prev = vOut
    }
  }

  "Out Vector" should "be of length 220" in {
    val rnd = new Random(System.currentTimeMillis())
    val A:DenseMatrix[Double]= DimensionalityReducer.getRandMatrix(20000000,4096);

    val vIn:Vector[Double] = Vector.fill(4096)(rnd.nextDouble)
    val vOut:Vector[Double] = DimensionalityReducer.getNewVector(vIn,A)

    vOut.size should be (220)
    vIn.size should be (4096)
  }
}
