/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.chain

import cc.factorie._
import cc.factorie.maths.ArrayOps
import cc.factorie.la._
import cc.factorie.optimize._
import cc.factorie.app.chain.infer._
import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import java.io._
import scala.collection.mutable
import org.junit.Assert._
import scala.collection.mutable.LinkedHashMap
import cc.factorie.util.{BinarySerializer, DoubleAccumulator}


//TODO We should add the ability to explictly permit and forbid label transitions
class ChainModel[Label<:LabeledMutableDiscreteVarWithTarget[_], Features<:CategoricalVectorVar[String], Token<:Observation[Token]]
(val labelDomain:CategoricalDomain[String],
 val featuresDomain:CategoricalVectorDomain[String],
 val labelToFeatures:Label=>Features,
 val labelToToken:Label=>Token,
 val tokenToLabel:Token=>Label) 
 (implicit lm:Manifest[Label], fm:Manifest[Features], tm:Manifest[Token])
extends ModelWithContext[IndexedSeq[Label]] //with Trainer[ChainModel[Label,Features,Token]]
with Parameters
{
  self =>
  val labelClass = lm.erasure
  val featureClass = fm.erasure
  val tokenClass = tm.erasure
  val bias = new DotFamilyWithStatistics1[Label] {
    factorName = "Label"
    val weights = Weights(new la.DenseTensor1(labelDomain.size))
  }
  val obs = new DotFamilyWithStatistics2[Label,Features] {
    factorName = "Label,Token"
    val weights = Weights(new la.DenseTensor2(labelDomain.size, featuresDomain.dimensionSize))
  }
  val markov = new DotFamilyWithStatistics2[Label,Label] {
    factorName = "Label,Label"
    val weights = Weights(new la.DenseTensor2(labelDomain.size, labelDomain.size))
  }
  val obsmarkov = new DotFamilyWithStatistics3[Label,Label,Features] {
    factorName = "Label,Label,Token"
    val weights = Weights(if (useObsMarkov) new la.DenseTensor3(labelDomain.size, labelDomain.size, featuresDomain.dimensionSize) else new la.DenseTensor3(1, 1, 1))
  }
  var useObsMarkov = false

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataOutputStream(stream)
    BinarySerializer.serialize(featuresDomain, dstream)
    BinarySerializer.serialize(labelDomain, dstream)
    BinarySerializer.serialize(this, dstream)
    dstream.close()
  }
  def deserialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val dstream = new DataInputStream(stream)
    BinarySerializer.deserialize(featuresDomain, dstream)
    BinarySerializer.deserialize(labelDomain, dstream)
    BinarySerializer.deserialize(this, dstream)
    dstream.close()
  }

  def factorsWithContext(labels:IndexedSeq[Label]): Iterable[Factor] = {
    val result = new ListBuffer[Factor]
    for (i <- 0 until labels.length) {
      result += bias.Factor(labels(i))
      result += obs.Factor(labels(i), labelToFeatures(labels(i)))
      if (i > 0) {
        result += markov.Factor(labels(i-1), labels(i))
        if (useObsMarkov) result += obsmarkov.Factor(labels(i-1), labels(i), labelToFeatures(labels(i)))
      }
    }
    result
  }
  def factors(variables: Iterable[Var]): Iterable[Factor] = variables match {
    case variables: IndexedSeq[Label] if variables.forall(v => labelClass.isAssignableFrom(v.getClass)) => factorsWithContext(variables)
    case variables: Seq[Label] if variables.forall(v => labelClass.isAssignableFrom(v.getClass)) => factorsWithContext(variables.toIndexedSeq)
    case _ => { val result = newFactorsCollection; variables.foreach(v => result ++= factors(v)); result }
  }
  override def factors(v: Var) = v match {
    case label:Label if label.getClass eq labelClass => {
      val result = new ArrayBuffer[Factor](4)
      result += bias.Factor(label)
      result += obs.Factor(label, labelToFeatures(label))
      val token = labelToToken(label)
      if (token.hasPrev) {
        result += markov.Factor(tokenToLabel(token.prev), label)
        if (useObsMarkov)
          result += obsmarkov.Factor(tokenToLabel(token.prev), label, labelToFeatures(label))
      }
      if (token.hasNext) {
        result += markov.Factor(label, tokenToLabel(token.next))
        if (useObsMarkov)
          result += obsmarkov.Factor(label, tokenToLabel(token.next), labelToFeatures(tokenToLabel(token.next)))
      }
      result
    }
  }
}


