package LSH.hashFunctions

import LSH.tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 9/26/16.
  */
class Hyperplane(k:Int) extends HashFunction(k) {

  var hyperPlanes = new ArrayBuffer[Vector[Float]]
  val rnd = new Random(System.currentTimeMillis())

  // Initialize k random hyperplanes for H[i]
  for(m <- 0 until k) {
    hyperPlanes += generateRandomV(4096)
  }

  def apply(v: Vector[Float]): String = {
    var sb : StringBuilder = new StringBuilder
    for(h <- hyperPlanes) {
      sb ++= hash(v,h).toString
    }
    // TODO Dont use string
    sb.toString()
  }
  def hash(v: Vector[Float], randomV: Vector[Float]): Int = {
    if (Distance.dotProduct(v, randomV) > 0) 1 else 0
  }

  // TODO check the seq structure
  def generateRandomV(size: Int): Vector[Float] = {
    val buf = new ArrayBuffer[Float]
    for (i <- 0 until size)
      buf += (if (rnd.nextGaussian() < 0) -1 else 1)

    buf.toVector
  }
}