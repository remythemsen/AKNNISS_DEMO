package preProcessing

import breeze.linalg.DenseMatrix
import tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

/**
  * Created by Chris on 9/20/16.
  */

object DimensionalityReducer{

  def getNewVector(x:Array[Float],A: DenseMatrix[Float]):Array[Float] = {
    val y=MatrixVectorProduct(x,A)//return Reduced Vector
    y
  }

  def getRandMatrix(n:Int, d:Int): DenseMatrix[Float] ={

    val epsilon=1// 0 <= epsilon <= 1
    val base2 = scala.math.log(2)
    val log2N = scala.math.log(n) / base2
    // m = new reduced dimension
    val m=((9*epsilon*log2N).toInt) + 2

    val A = DenseMatrix.rand(m, d, breeze.stats.distributions.Gaussian(0, 1))
    val M=normalizeMatrix(A)
    M
  }

  def MatrixVectorProduct(x:Array[Float],A:DenseMatrix[Float]):Array[Float]={
    //A*xw
    val reduced:Array[Float] = new Array(A.rows)
    // Reusable array
    val b:Array[Float] = new Array(x.length)

    for(i<-0 until A.rows){
      for(j<-x.indices){
       b(j) = A(i,j) // filling b with new values
      }
      reduced(i) = Distance.parDotProduct(b,x)
    }
    reduced
  }

  def normalizeMatrix(A:DenseMatrix[Double]):DenseMatrix[Float]={
    val buffer= new ArrayBuffer[Float]

    val B:DenseMatrix[Float] = DenseMatrix.zeros(A.rows, A.cols)

    for(i<-0 until A.rows){
      val b = new ArrayBuffer[Double]
      for(j<-0 until A.cols){
        b+=A(i,j) // Note this conversion
      }
      val l= sqrt(b.map { case (x) => pow(x, 2) }.sum)
      for(c <-0 until A.cols){
        B(i,c) = (b(c)/l).toFloat
      }
    }
    B
  }

}
