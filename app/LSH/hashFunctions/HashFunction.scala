package LSH.hashFunctions

import scala.util.Random

/**
  * Created by remeeh on 10/15/16.
  */
@SerialVersionUID(100L)
abstract class HashFunction(k:Int, rnd:() => Random) extends Serializable {
  def apply(v:Vector[Float]) : String
}

