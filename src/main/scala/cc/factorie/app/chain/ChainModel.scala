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
import Factorie._
import scala.collection.mutable.{ListBuffer,ArrayBuffer}

import java.io.{InputStream, OutputStream, DataInputStream, DataOutputStream}
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.{CategoricalVectorVar, LabeledMutableDiscreteVarWithTarget}
import scala.reflect.ClassTag


//TODO We should add the ability to explictly permit and forbid label transitions
class ChainModel[Label<:LabeledMutableDiscreteVarWithTarget, Features<:CategoricalVectorVar[String], Token<:Observation[Token]]
(val labelDomain:CategoricalDomain[String],
 val featuresDomain:CategoricalVectorDomain[String],
 val labelToFeatures:Label=>Features,
 val labelToToken:Label=>Token,
 val tokenToLabel:Token=>Label) 
 (implicit lm:ClassTag[Label], fm:ClassTag[Features], tm:ClassTag[Token])
extends Model with Parameters
{
  self =>
  val labelClass = lm.runtimeClass
  val featureClass = fm.runtimeClass
  val tokenClass = tm.runtimeClass
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

  def factors(variables:Iterable[Var]): Iterable[Factor] = {
    val result = new ListBuffer[Factor]
    variables match {
      case labels: Iterable[Label] if variables.forall(v => labelClass.isAssignableFrom(v.getClass)) =>
        var prevLabel: Label = null.asInstanceOf[Label]
        for (label <- labels) {
          result += bias.Factor(label)
          result += obs.Factor(label, labelToFeatures(label))
          if (prevLabel ne null) {
            result += markov.Factor(prevLabel, label)
            if (useObsMarkov) result += obsmarkov.Factor(prevLabel, label, labelToFeatures(label))
          }
          prevLabel = label
        }
    }
    result
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


