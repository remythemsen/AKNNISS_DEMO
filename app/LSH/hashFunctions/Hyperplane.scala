package LSH.hashFunctions

import tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Hyperplane(k:Int, rndf:() => Random) extends HashFunction(k, rndf) {
  val rnd = rndf()

  val hyperPlanes = for {
    i <- 0 until k
    hp <- List(generateRandomV(220))
  } yield hp

  def apply(v:IndexedSeq[Float]) = {
    val res = for {
      hp <- hyperPlanes
      r <- List(hash(v,hp))
    } yield r
    res.mkString
  }

  def hash(v: IndexedSeq[Float], randomV: IndexedSeq[Float]): Int = {
    if (Distance.dotProduct(v, randomV) > 0) 1 else 0
  }

  def generateRandomV(size: Int) = {
    for {
      i <- 0 until size
      c <- List[Float]({if (rnd.nextBoolean()) -1 else 1})
    } yield c
  }
}

