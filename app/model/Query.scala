package model

import java.io.{FileInputStream, ObjectInputStream}
import LSH.structures.LSHStructure
import tools.Cosine

object Query {
  // Load LSHStructure
  val ois = new ObjectInputStream(new FileInputStream("out/structures/Hyperplane_1_1_random.lshstructure"))
  println("Loading started!")
  val lshs: LSHStructure = ois.readObject.asInstanceOf[LSHStructure]
  ois.close()
  println("Structure Loaded")

  // return the result generated by
  def getResults(imageId:String, range:Double) : (String, List[((String, IndexedSeq[Float]), Double)]) = {
    val fVector = lshs.findVectorById(imageId)
    val tres = time {
      lshs.query(fVector, range, Cosine)
    }
    val res2 = tres._1.zip(tres._1.map(x => Cosine.measure(fVector._2, x._2))).sortBy(_._2)
    (tres._2, res2.toList)
  }

  def time[R](block: => R) = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    (result, (t1 - t0) + "ms")
  }

}
