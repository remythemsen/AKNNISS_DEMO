package tools

import scala.util.Random

import org.scalatest._

/**
  * Created by remeeh on 07-11-2016.
  */
class DistanceSpec extends FlatSpec with Matchers {
  "Cosine Measure" should "have cosine distance of 0 when comparing two identical vectors" in {
    val rnd = new Random(System.currentTimeMillis())
    val v1:IndexedSeq[Float] = IndexedSeq.fill(4096)(rnd.nextFloat)
    val v2 = v1

    Cosine.measure(v1, v2) should be (0.0)
  }

}
