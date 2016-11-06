package LSH.structures

import tools.Distance
import scala.collection.mutable.ArrayBuffer

/**
  * Structure of hashtables meant for querying k near neighbors of v
  */

@SerialVersionUID(100L)
class LSHStructure(hts:IndexedSeq[HashTable]) extends Serializable {
  val hashTables = hts

  // Building lookup map
  val items = this.hashTables.head.table.valuesIterator.flatten.toList
  val lookupMap = Map(items map { s => (s._1, s._2)} : _*)

  /**
    * Takes a query vector and finds k near neighbours in the LSH Structure
    *
    * @param v Query vector
    * @param r Accepting neighbours within range
    * @return set of k near neighbours
    */

  def query(v:(String, Vector[Float]), r:Double, dist:Distance) : IndexedSeq[(String, Vector[Float])] = {
    val result = for {
      h <- hashTables
      r <- h.query(v._2)
    } yield r

    result.distinct.filter(x => dist.measure(x._2, v._2) < r)
  }

  def findVectorById(id:String) = {
    (id, lookupMap.get(id).head)
  }
}

