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


package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import app.classify
import app.classify.{LabelList, ID3DecisionTreeTrainer}
import la.Tensor

/**A document classifier that uses Decision Trees.
    Note that it also does not use any of the facilities of cc.factorie.app.classify.document */
object DocumentClassifier4 {

  object DocumentDomain extends CategoricalTensorDomain[String]
  class Document(file: File) extends BinaryFeatureVectorVariable[String] {
    def domain = DocumentDomain
    var label = new Label(file.getParentFile.getName, this)
    // Read file, tokenize with word regular expression, and add all matches to this BinaryFeatureVectorVariable
    "\\w+".r.findAllIn(Source.fromFile(file, "ISO-8859-1").mkString).foreach(regexMatch => this += regexMatch.toString)
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(name: String, val document: Document) extends LabelVariable(name) {
    def domain = LabelDomain
  }

  def main(args: Array[String]): Unit = {

    if (args.length < 2)
      throw new Error("Usage: directory_class1 directory_class2 ...\nYou must specify at least two directories containing text files for classification.")

    // Read data and create Variables
    var docLabels = new classify.LabelList[Label,Document](_.document)
    for (directory <- args) {
      val directoryFile = new File(directory)
      if (!directoryFile.exists) throw new IllegalArgumentException("Directory " + directory + " does not exist.")
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        //println ("Directory "+directory+" File "+file+" documents.size "+documents.size)
        docLabels += new Document(file).label
      }
    }

    // Make a test/train split
    val (testSet, trainSet) = docLabels.shuffle.split(0.5)
    val trainLabels = new classify.LabelList[Label,Document](trainSet, _.document)
    val testLabels = new classify.LabelList[Label,Document](testSet, _.document)

    val start = System.currentTimeMillis
    // Train decision tree
    val classifier = new ID3DecisionTreeTrainer().train(trainLabels)

    // Test decision tree

    val testTrial = new classify.Trial[Label](classifier)
    testTrial ++= testLabels

    val trainTrial = new classify.Trial[Label](classifier)
    trainTrial ++= trainLabels

    println("Train accuracy = " + trainTrial.accuracy)
    println("Test  accuracy = " + testTrial.accuracy)
    println("Number of ms to train/test: " + (System.currentTimeMillis - start))
    println(classifier.model
      .asInstanceOf[DecisionTreeStatistics2Base[Label#ValueType,Document#ValueType]]
      .splittingFeatures.map(DocumentDomain.dimensionDomain.categories(_)))
  }
}



