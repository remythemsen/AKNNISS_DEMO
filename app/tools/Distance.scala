package tools

import scala.math.{abs, pow, sqrt}

trait Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float
}

object Distance {
  def magnitude(x: Array[Float]): Double = {
    math.sqrt(x map(i => i*i) sum)
  }
  def ddotProduct(x: Array[Double], y: Array[Double]): Double = {
    (x zip y).map { case (x, y) => (y * x) }.sum
  }
  def dotProduct(x: Array[Float], y: Array[Float]): Float = {
    (for((a, b) <- x zip y) yield a * b) sum
  }
  def normalize(x:Array[Float]):Array[Float]={
    val m=magnitude(x)
    (x).map { case (x) =>(x/m).toFloat}
  }
}

case object Cosine extends Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float = {
    val res = 1-(Distance.dotProduct(x, y)/(Distance.magnitude(x) * Distance.magnitude(y))).toFloat
    // normalize result:
    res / 2
  }

}

case object Euclidean extends Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float = {
    sqrt((x zip y).map { case (x, y) => pow(y - x, 2) }.sum).toFloat
  }
}

case object Manhattan extends Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float = {
    (x zip y).map { case (x, y) => abs(y - x) }.sum
  }

}

case object LInfinityNorm extends Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float = {
    (x zip y).map { case (x, y) => abs(y - x) }.max
  }
}

case object Hamming extends Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float = {
    (x zip y).count { case (x, y) => (y != x) }
  }
}

