package LSH.hashFunctions

import java.util.Random

import tools.Distance

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Chris on 9/26/16.
  */

class CrossPolytope(k:Int) extends HashFunction(k) {

  def isSparse(x: Vector[Double]): Boolean = {
    //return true if sparse, otherwise false
    val m=x.size;
    var count=0
    for(i<-0 until x.size){
      if(count >= m/2) false
      if (x(i)!=0) count+=1
    }
    true
  }

  def featureHashing(x: Vector[Double]): Vector[Double]  = {
    // for sparse vector x, return x'
    // apply a linear map x ⟶ Sx
    val d=x.size
    val dPrime=d/2
    //val dPrime=(log(d)/log(2)).toInt
    val S = generateRandomSparseMatrixS(x.size, dPrime, System.currentTimeMillis())
    val newX: Vector[Double] = MatrixVectorProduct(S,x)

    newX
  }

  def generateRandomSparseMatrixS(oldD: Int, newD: Int, seed: Long): Array[Array[Double]] = {
    // S - random sparse d x d’ matrix, whose columns have one non-zero, ±1 entry sampled uniformly)
    val rand = new Random(System.currentTimeMillis())
    val matrixS = Array.ofDim[Double](newD, oldD)

    for(i<-0 until oldD){
      val j = rand.nextInt(newD)
      matrixS(j)(i)=(if (rand.nextBoolean() == 0) -1 else 1)
    }
    matrixS
  }

  def generateRandomDiagonalMatrixD(size: Int, seed: Long): Array[Array[Double]] = {
    // D - random diagonal {±1} matrix (used for “flipping signs”)
    val rnd = new Random(seed)
    //Generate Diagonal matrix with {±1}
    val matrixD = Array.ofDim[Double](size, size)
    for(i<-0 until size){
      for(j<-0 until size){
        if(i==j){matrixD(i)(j)=(if (rnd.nextBoolean() == 0) -1 else 1)}
        else{matrixD(i)(j)==0}
      }
    }
    matrixD
  }


  def generateHadamard (x:Int,mat:Array[Array[Double]]):Unit=
  {
    generateHadamard(mat, 0,0,mat.length, 1.0); //overloading, assuming mat.length is pow of 2
  }
  def generateHadamard (mat:Array[Array[Double]], top:Int, left:Int, size:Int, sign:Double): Unit=
  {
    if (size == 1.0)
      mat(top)(left) = sign;
    else
    {
      generateHadamard (mat, top, left, size/2, sign);
      generateHadamard (mat, top+size/2, left, size/2, sign);
      generateHadamard (mat, top, left+size/2, size/2, sign);
      generateHadamard (mat, top+size/2, left+size/2, size/2, (-1)*sign);
    }
  }


  def MatrixVectorProduct(A:Array[Array[Double]],x:Vector[Double]):Vector[Double]={
    //A*xw
    val buffer= new ArrayBuffer[Double]

    for(i<-0 until A.size){
      val b = new ArrayBuffer[Double]
      for(j<-0 until x.size){
        b+=A(i)(j)
      }
      buffer+=Distance.dotProduct(b.toVector,x)
    }
    // return the new vector with reduced dimensions
    buffer.toVector
  }

  // compute pseudorandom rotation: Fast Hadamard Transform
  def computeHash(x: Vector[Double]): Int = {
    // y = HD1HD2HD3x // matrix multiplication
    val y = pseudoRandomRotation(x)

    var max = 0.0
    var indexOfMax = 0
    for(i<-0 until y.size){
      if(Math.abs(y(i)) > max) max = y(i)
      indexOfMax = i
    }

    if(max > 0)  2*indexOfMax -1
    else  2*indexOfMax - 2
  }

  def pseudoRandomRotation(x:Vector[Double]): Vector[Double] ={
    val D1 = generateRandomDiagonalMatrixD(x.size, System.currentTimeMillis())
    val D2 = generateRandomDiagonalMatrixD(x.size, System.currentTimeMillis())
    val D3 = generateRandomDiagonalMatrixD(x.size, System.currentTimeMillis())

    val H  =Array.ofDim[Double](x.size,x.size)
    generateHadamard(x.size, H)

    val y = MatrixVectorProduct(H,MatrixVectorProduct(D3,MatrixVectorProduct(H,MatrixVectorProduct(D2,MatrixVectorProduct(H,MatrixVectorProduct(D1,x))))))
    y
  }


  def apply(x: Vector[Double]): String = {
    computeHash(x).toString
  }

}
