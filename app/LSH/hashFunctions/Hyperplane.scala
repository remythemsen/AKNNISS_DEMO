package LSH.hashFunctions

import tools.Distance
import scala.util.Random

class Hyperplane(k:Int, rndf:() => Random) extends HashFunction(k, rndf) {
  val rnd = rndf()

  val hyperPlanes = for {
    i <- 0 until k
    hp <- List(generateRandomV(220))
  } yield hp

  def apply(v:Array[Float]) = {
    val res = for {
      hp <- hyperPlanes
      r <- List(hash(v,hp))
    } yield r
    res.mkString
  }

  def hash(v: Array[Float], randomV: Array[Float]): Int = {
    if (Distance.dotProduct(v, randomV) > 0) 1 else 0
  }

  def generateRandomV(size: Int) : Array[Float] = {
    val set = for {
      i <- 0 until size
      c <- Array[Float]({if (rnd.nextBoolean()) -1 else 1})
    } yield c

    set.toArray
  }
}

