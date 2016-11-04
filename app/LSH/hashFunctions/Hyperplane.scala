package LSH.hashFunctions

import tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 9/26/16.
  */
class Hyperplane(k:Int, rndf:() => Random) extends HashFunction(k, rndf) {
  val rnd = rndf()

  var hyperPlanes = new ArrayBuffer[Vector[Double]]

  // Initialize k random hyperplanes for H[i]
  for(m <- 0 until k) {
    hyperPlanes += generateRandomV(225)
  }

  def apply(v: Vector[Double]): String = {
    var sb : StringBuilder = new StringBuilder
    for(h <- hyperPlanes) {
      sb ++= hash(v,h).toString
    }
    // TODO Dont use string
    sb.toString()
  }
  def hash(v: Vector[Double], randomV: Vector[Double]): Int = {
    if (Distance.dotProduct(v, randomV) > 0) 1 else 0
  }

  // TODO check the seq structure
  def generateRandomV(size: Int): Vector[Double] = {
    val buf = new ArrayBuffer[Double]
    for (i <- 0 until size)
      buf += (if (rnd.nextBoolean()) -1 else 1)

    buf.toVector
  }
}

