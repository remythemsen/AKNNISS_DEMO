package preProcessing

import breeze.linalg.DenseMatrix
import org.scalatest._
import scala.util.Random

class DimensionalityReducerSpec extends FlatSpec with Matchers {
  "All outvectors" should "have the same length" in {
    val rnd = new Random(System.currentTimeMillis())
    val A:DenseMatrix[Double]= DimensionalityReducer.getRandMatrix(20000000,4096);

    for(i <- 0 until 1000) {
      val vIn:Vector[Double] = Vector.fill(4096)(rnd.nextDouble)
      val vOut:Vector[Double] = DimensionalityReducer.getNewVector(vIn,A)
      (vIn.size == vOut.size) should be (true)
    }
  }
}
