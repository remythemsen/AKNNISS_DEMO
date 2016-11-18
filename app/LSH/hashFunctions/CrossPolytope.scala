package LSH.hashFunctions

import tools.Distance
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by Chris on 9/26/16.
  */

class CrossPolytope(k: Int, rndf:() => Random) extends HashFunction(k, rndf) {

  val rnd = rndf()

  // TODO: remove hardcoded value
  val ds=k*3

  val diagonals=new Array[Vector[Float]](ds)
  for(i<-0 until ds){
    diagonals(i) = generateRDV(256,rnd.nextLong())
  }


  def generateRDV(size: Int, seed: Long): Vector[Float] = {
    // D - random diagonal {±1} matrix (used for “flipping signs”)
    val rnd = new Random(seed)
    //Generate Diagonal matrix with {±1}
    val b=new ArrayBuffer[Float]()
    for(i<-0 until size){
      if (rnd.nextBoolean()){ b += -1}
      else{b+= 1}
    }

    b.toVector
  }


  def VectorMultiplication(A:Vector[Float],x:IndexedSeq[Float]):IndexedSeq[Float]={
    // TODO Chris & Rox, please double check the double to float conversion here
    val b = new ArrayBuffer[Float]
    for(i<-0 until x.size){
      b+= (A(i)*x(i))
    }
    b.toIndexedSeq
  }

  // compute pseudorandom rotation: Fast Hadamard Transform
  def computeHash(x: IndexedSeq[Float]): Array[Int] = {
    // y = HD1HD2HD3x // matrix multiplication
    var b=new Array[Float](x.size)
    println("SIZE",b.size,x.size)
    val H = hadamardTransformation(x,0,x.size-1,b)
    val arr = new Array[Int](k)
    var index=0
    for(i<-0 until k) {
      val y = pseudoRandomRotation(H,x, index)
      var max = 0.0
      var indexOfMax = 0
      for (i <- 0 until y.size) {
        if (Math.abs(y(i)) > max) {
          max = y(i)
          indexOfMax = i
        }
      }

      if (max > 0) {arr(i)=2 * indexOfMax - 1}
      else {arr(i)=2 * indexOfMax - 2}
      index+=3
    }

    arr
  }

  //TODO Create the method for Hadamard transformation !!!
  //  def hadamardTransformation(x:IndexedSeq[Float]):Vector[Float]={
  //    null
  //  }

  def hadamardTransformation(a:IndexedSeq[Float],low:Int,high:Int,y:Array[Float]):Vector[Float]={

    if(high-low>0)
    {
      var middle=(low+high)/2
      //System.out.println(middle);
      var c=1;
      for(i<-low until middle+1){
        y(i) = a(i)+a(middle+c)
        c+=1
      }
      var m=0;
      for(j<-middle+1 until high+1){
        y(j)= -a(j) + a(low+m)
        m+=1
      }
      var b=new Array[Float](a.size)
      for(i<-0 until a.size){
        b(i)= y(i)
      }
      //print(y);
      hadamardTransformation(b.toIndexedSeq,low,middle,y);
      hadamardTransformation(b.toIndexedSeq,middle+1,high,y);

    }
    y.toVector;
  }
  //Rotation
  def pseudoRandomRotation(H:Vector[Float],x: IndexedSeq[Float],i:Int): IndexedSeq[Float] ={
    VectorMultiplication(H,VectorMultiplication(diagonals(i),VectorMultiplication(H,VectorMultiplication(diagonals(i+1),VectorMultiplication(H,VectorMultiplication(diagonals(i+2),x))))))
  }

  def apply(x: IndexedSeq[Float]): String = {
    val hash = computeHash(x)
    //hash.toString()
    convertString(hash)
  }

  def convertString(x:Array[Int]):String={
    var str =""
    for(i<-0 until x.size){
      if(x(i)>99){str +=x(i)}
      else{
        if(x(i)>9){str+="0"+x(i)}
        else{str+="00"+x(i)}
      }
    }
    //println(str)
    str
  }

}
