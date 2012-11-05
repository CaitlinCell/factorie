package cc.factorie
import cc.factorie.la._

object TestSampleRank {
  object LabelDomain extends CategoricalDomain[String]
  class Label(s:String, val instance:Instance) extends LabeledCategoricalVariable(s) { def domain = LabelDomain }
  object InstanceDomain extends CategoricalTensorDomain[String]
  class Instance(labelString:String) extends BinaryFeatureVectorVariable[String] {
    def domain = InstanceDomain
    val label = new Label(labelString, this)
    // Add features that coorespond to label exactly
    println("New Instance with Label "+labelString)
    this += "f1"+labelString; println("TestSampleRank features "+tensor+" intArray "+tensor.asInstanceOf[SparseBinaryTensorLike1].toIntArray.toSeq)
    this += "f2"+labelString; println("TestSampleRank features "+tensor+" intArray "+tensor.asInstanceOf[SparseBinaryTensorLike1].toIntArray.toSeq)
    this += "f3"+labelString; println("TestSampleRank features "+tensor+" intArray "+tensor.asInstanceOf[SparseBinaryTensorLike1].toIntArray.toSeq)
  }
  object model extends DotTemplateWithStatistics2[Label,Instance] {
    lazy val weights = new la.DenseTensor2(LabelDomain.size, InstanceDomain.dimensionSize)
    def unroll1(l:Label) = Factor(l, l.instance)
    def unroll2(i:Instance) = Factor(i.label, i)
  }
  
  def main(args:Array[String]): Unit = {

    // Test Tensor index arithmetic
    for (trials <- 1 to 100) {
      val dim1 = random.nextInt(20)+1
      val dim2 = random.nextInt(20)+1
      val dim3 = random.nextInt(20)+1
      val dim4 = random.nextInt(20)+1
      println(List(dim1, dim2, dim3, dim4))
      var v = 0.0; var rdim1 = 0; var rdim2 = 0; var rdim3 = 0; var rdim4 = 0

      val t2 = new DenseTensor2(dim1,dim2)
      v = 0.0; for (i <- 0 until dim1; j <- 0 until dim2) { t2(i, j) = v; v += 1.0 }
      v = 0.0; for (i <- 0 until dim1*dim2) { assert(t2(i) == v, "dim1="+dim1+" dim2="+dim2+" i="+i+" v="+v+" t(i)="+t2(i)+"\n"+t2); v += 1.0 }
      rdim1 = random.nextInt(dim1)
      rdim2 = random.nextInt(dim2)
      val t2i = t2.singleIndex(rdim1, rdim2)
      assert(t2.multiIndex(t2i) == (rdim1, rdim2))
      
      val t3 = new DenseTensor3(dim1,dim2,dim3)
      v = 0.0; for (i <- 0 until dim1; j <- 0 until dim2; k <- 0 until dim3) { t3(i, j, k) = v; v += 1.0 }
      v = 0.0; for (i <- 0 until dim1*dim2*dim3) { assert(t3(i) == v); v += 1.0 }
      rdim1 = random.nextInt(dim1)
      rdim2 = random.nextInt(dim2)
      rdim3 = random.nextInt(dim3)
      val t3i = t3.singleIndex(rdim1, rdim2, rdim3)
      assert(t3.multiIndex(t3i) == (rdim1, rdim2, rdim3))

      val t4 = new DenseTensor4(dim1,dim2,dim3,dim4)
      v = 0.0; for (i <- 0 until dim1; j <- 0 until dim2; k <- 0 until dim3; l <- 0 until dim4) { t4(i, j, k, l) = v; v += 1.0 }
      v = 0.0; for (i <- 0 until dim1*dim2*dim3*dim4) { assert(t4(i) == v); v += 1.0 }
      rdim1 = random.nextInt(dim1)
      rdim2 = random.nextInt(dim2)
      rdim3 = random.nextInt(dim3)
      rdim4 = random.nextInt(dim4)
      val t4i = t4.singleIndex(rdim1, rdim2, rdim3, rdim4)
      assert(t4.multiIndex(t4i) == (rdim1, rdim2, rdim3, rdim4))
    }
    
    
    val labels = List("n", "y").map(s => new Instance(s)).map(_.label)
    println("feature domain: "+InstanceDomain.dimensionDomain.mkString(" "))
    println("feature tensors:\n"+labels.map(l => l.instance.tensor.toString+"\n"))
    val learner = new optimize.SampleRankTrainer(new GibbsSampler(model, HammingObjective), new cc.factorie.optimize.StepwiseGradientAscent)
    //learner.logLevel = 10
    learner.processContexts(labels)
    labels.foreach(l => l.set(0)(null)); println("Set to 0")
    labels.foreach(l => println("feature="+l.instance.tensor+" value="+l.categoryValue+" target="+l.target.categoryValue+" score="+model.currentScore(l)))
    labels.foreach(l => l.set(1)(null)); println("Set to 1")
    labels.foreach(l => println("feature="+l.instance.tensor+" value="+l.categoryValue+" target="+l.target.categoryValue+" score="+model.currentScore(l)))
    MaximizeDiscrete(labels, model); println("Set to max")
    labels.foreach(l => println("feature="+l.instance.tensor+" value="+l.categoryValue+" target="+l.target.categoryValue+" score="+model.currentScore(l)))
    println("Train accuracy "+labels.map(l => HammingObjective.currentScore(l)).sum / labels.length)
  }
}