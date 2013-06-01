package cc.factorie.optimize

import cc.factorie._
import la._
import util._

/**
 * User: apassos
 * Date: 4/8/13
 * Time: 5:54 PM
 */

// This implements the AdaGrad algorithm with efficient dual averaging updates and support for l1 and l2 regularization
class AdaGradRDA(val delta: Double = 0.1, val rate: Double = 0.1, val l1: Double = 0.0, val l2: Double = 0.0) extends GradientOptimizer {
  var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double) {
    if (!initialized) initializeWeights(weights)
    weights.+=(gradient)
  }
  def initializeWeights(weights: WeightsSet): Unit = {
    for (key <- weights.keys) {
      key.value match {
        case t: Tensor1 => weights(key) = new AdaGradRDATensor1(t.length, delta, rate, l1, l2)
        case t: Tensor2 => weights(key) = new AdaGradRDATensor2(t.dim1, t.dim2, delta, rate, l1, l2)
        case t: Tensor3 => weights(key) = new AdaGradRDATensor3(t.dim1, t.dim2, t.dim3, delta, rate, l1, l2)
        case t: Tensor4 => weights(key) = new AdaGradRDATensor4(t.dim1, t.dim2, t.dim3, t.dim4, delta, rate, l1, l2)
      }
    }
    initialized = true
  }

  def finalizeWeights(weights: WeightsSet): Unit =
    for (key <- weights.keys) {
      val scaledTensor = key.value
      weights(key) = key.newBlankTensor
      scaledTensor.foreachElement((i, v) => key.value(i) = v)
    }

  def reset() {}
  def isConverged = false

  private trait AdaGradRDATensor extends Tensor  {
    def activeDomain = new RangeIntSeq(0, length)
    val gradients = Array.fill(length)(0.0)
    val gradSquares = Array.fill(length)(0.0)
    var t = 0
    val delta: Double
    val rate: Double
    val l1: Double
    val l2: Double

    // can we use += here?
    def copyToDense[D <: DenseTensor](d: D): D = {
      var i = 0
      assert(length == d.length)
      while (i < length) {
        d(i) = apply(i)
        i += 1
      }
      d
    }

    override def update(i: Int, v: Double) { throw new Error("DualAveragingTensors can't be updated") }
    override def apply(i: Int): Double = {
      if (gradSquares(i) == 0.0) 0.0
      else {
        val h = (1.0/rate) *(math.sqrt(gradSquares(i)) + delta) + t*l2
        val t1 = 1.0/h
        t1 * ISTAHelper.truncate(gradients(i), t*l1)
      }
    }

    override def +=(ds: DoubleSeq, factor: Double) {
      t += 1
      ds match {
        case o: SparseTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          val values = o._valuesSeq
          var i = 0
          while (i < len) {
            gradients(indices(i)) += values(i)*factor
            gradSquares(indices(i)) += values(i)*values(i)*factor*factor
            i += 1
          }
        case o: DenseTensor =>
          val arr = o.asArray
          var i = 0
          while (i < arr.length) {
            gradients(i) += arr(i)*factor
            gradSquares(i) += arr(i)*arr(i)*factor*factor
            i += 1
          }
        case _ => throw new Error("no match statement for " + ds.getClass.getName)
      }
    }

    override def dot(ds: DoubleSeq) = {
      var res = 0.0
      ds.foreachActiveElement((i, x) => res += apply(i)*x)
      res
    }
    def copy: Tensor = throw new Error("Method copy not defined on class "+getClass.getName)
    def blankCopy: Tensor = throw new Error("Method blankCopy not defined on class "+getClass.getName)
    def +=(i: Int, v: Double): Unit = throw new Error("You should add tensors all at once to the AdaGradRDATensor")
    def zero(): Unit = for (i <- 0 until length) { gradients(i) = 0; gradSquares(i) = 0 }
  }

  private class AdaGradRDATensor1(val dim1: Int, val rate: Double, val delta: Double, val l1: Double, val l2: Double) extends AdaGradRDATensor with Tensor1 {
    def isDense = false
    override def copy = copyToDense(new DenseTensor1(dim1))
  }
  private class AdaGradRDATensor2(val dim1: Int, val dim2: Int, val rate: Double, val delta: Double, val l1: Double, val l2: Double) extends AdaGradRDATensor with Tensor2 {

    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def isDense = false
    override def copy = copyToDense(new DenseTensor2(dim1, dim2))

    override def *(t: Tensor1): Tensor1 = {
  //    assert(dim2 == t.dimensions.reduce(_ * _), "Dimensions don't match: " + dim2 + " " + t.dimensions)
      val newT = new DenseTensor1(dim1)
      val newArray = newT.asArray
      t match {
        case t: DenseTensor =>
          val tArr = t.asArray
          var col = 0
          while (col < tArr.length) {
            val v = tArr(col)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (apply(offset + col) * v)
              row += 1
            }
            col += 1
          }
        case t: SparseTensor =>
          val tActiveDomainSize = t.activeDomainSize
          val tIndices = t._indices
          val tValues = t._valuesSeq
          var ti = 0
          while (ti < tActiveDomainSize) {
            val col = tIndices(ti)
            val v = tValues(ti)
            var row = 0
            while (row < dim1) {
              val offset = row * dim2
              newArray(row) += (apply(offset + col) * v)
              row += 1
            }
            ti += 1
          }
        case _ =>
          throw new Error("tensor type neither dense nor sparse: " + t.getClass.getName)
      }
      newT
    }
  }
  private class AdaGradRDATensor3(val dim1: Int, val dim2: Int, val dim3: Int, val rate: Double, val delta: Double, val l1: Double, val l2: Double) extends AdaGradRDATensor with Tensor3 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    override def copy = copyToDense(new DenseTensor3(dim1, dim2, dim3))
  }
  private class AdaGradRDATensor4(val dim1: Int, val dim2: Int, val dim3: Int, val dim4: Int, val rate: Double, val delta: Double, val l1: Double, val l2: Double) extends AdaGradRDATensor with Tensor4 {
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain4 = new RangeIntSeq(0, dim4)
    override def copy = copyToDense(new DenseTensor4(dim1, dim2, dim3, dim4))
  }
}

// helper object for Iterated Shrinkage and Thresholding for l1 regularization
object ISTAHelper {
  @inline final def truncate(x0: Double, l1: Double): Double = {
    if (x0 > l1)
      x0-l1
    else if (x0 < -l1)
      x0+l1
    else 0.0
  }
}