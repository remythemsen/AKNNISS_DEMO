package preProcessing

import breeze.linalg.DenseMatrix
import tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

/**
  * Created by Chris on 9/20/16.
  */

object DimensionalityReducer{

  def getNewVector(x:Array[Double],A: DenseMatrix[Double]):Array[Double] = {
    val y=MatrixVectorProduct(x,A)//return Reduced Vector
    y
  }

  def getRandMatrix(n:Int, d:Int): DenseMatrix[Double] ={

    val epsilon=1// 0 <= epsilon <= 1
    val base2 = scala.math.log(2)
    val log2N = scala.math.log(n) / base2
    // m = new reduced dimension
    val m=((9*epsilon*log2N).toInt) + 2

    val A = DenseMatrix.rand(m, d, breeze.stats.distributions.Gaussian(0, 1))
    val M=normalizeMatrix(A)
    M
  }

  def MatrixVectorProduct(x:Array[Double],A:DenseMatrix[Double]):Array[Double]={
    //A*xw
    val buffer:Array[Double] = new Array(A.rows)
    for(i<-0 until A.rows){
      var b:Array[Double] = new Array(x.size)
      for(j<-0 until x.size){
       b(j) = A(i,j)
        //TODO calc dot product here
      }
       buffer(i)=Distance.ddotProduct(b,x)
    }

    buffer
  }

  def normalizeMatrix(A:DenseMatrix[Double]):DenseMatrix[Double]={
    val buffer= new ArrayBuffer[Double]

    for(i<-0 until A.rows){
      val b = new ArrayBuffer[Double]
      for(j<-0 until A.cols){
        b+=A(i,j)
      }

      val l= sqrt((b).map { case (x) => pow(x, 2) }.sum)
      for(c <-0 until A.cols){
        A(i,c)=b(c)/l
      }
    }

    A
  }

}
