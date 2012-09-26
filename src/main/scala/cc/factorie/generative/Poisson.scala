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

package cc.factorie.generative
import cc.factorie._

object Poisson extends GenerativeFamily2[IntegerVar,DoubleVar] {
  case class Factor(override val _1:IntegerVar, override val _2:DoubleVar) extends super.Factor(_1, _2) {
    def pr(k:Int, mean:Double): Double = math.pow(mean, k) * math.exp(-mean) / maths.factorial(k)
    def pr(s:Statistics): Double = pr(s._1, s._2)
    def sampledValue(mean:Double): Int = maths.nextPoisson(mean)(cc.factorie.random).toInt
    def sampledValue(s:Statistics): Int = sampledValue(s._2)
  }
  def newFactor(a:IntegerVar, b:DoubleVar) = Factor(a, b)
}
