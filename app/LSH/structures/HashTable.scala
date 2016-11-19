package LSH.structures

import LSH.hashFunctions.HashFunction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 10/15/16.
  */
@SerialVersionUID(100L)
class HashTable(f:() => HashFunction) extends Serializable {
  // internal Mutable HashMap
  val table = new mutable.HashMap[String, ArrayBuffer[(String, Array[Float])]]()

  // internal Hash function
  val hf = f()

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:(String, Array[Float])) : Unit = {
    val key = hf(v._2)
    val value = {
      if(table.contains(key)) table(key)++ArrayBuffer(v)
      else ArrayBuffer(v)
    }
    table += (key -> value)
  }

  /**
    * @param v a query point
    * @return a list of vectors with same key as v
    */
  def query(v:Array[Float]) : ArrayBuffer[(String, Array[Float])] = {
    val key = hf(v)
    table(key)
  }

}

