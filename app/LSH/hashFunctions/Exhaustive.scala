package LSH.hashFunctions

import scala.util.Random

class Exhaustive(k:Int, rndf:() => Random) extends HashFunction(k, rndf) {
  val rnd = rndf()

  def apply(v:Vector[Float]) = {
    "1"
  }
}

