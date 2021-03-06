package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.util.{TopN,TopEntry}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.CategoricalDomain

class TopicPhraseCounts(numTopics:Int, multiWordOnly:Boolean = true) {
  
  private val counts = Array.tabulate(numTopics)(i => new CategoricalDomain[String])
  counts.foreach(_.gatherCounts = true) // Turn on counting so each call to "index" increments that string's count
  
  def apply(zi:Int): CategoricalDomain[String] = counts(zi)

  //val lengths = new ArrayBuffer[Int]

  def +=(doc:Doc): Unit = {
    var prevzi = -1
    val sb = new StringBuffer
    val ws = doc.ws
    val zs = doc.zs
    def addThenReset(s:String): Unit = {
      if (!multiWordOnly || s.contains("_")) counts(prevzi).index(sb.toString)
      //println("phrase="+sb.toString)
      sb.setLength(0)
    }
    //println("TopicPhraseCounts.+="+doc.breaks+" len="+ws.length+"  "+doc.ws.categoryValues.mkString(" "))
    for (i <- 0 until ws.length) {
      //if (i+1 < ws.length && (ws.categoryValue(i) == "logistic" || ws.categoryValue(i) == "Logistic")) println("@"+i+" Logistic:"+zs.intValue(i)+" "+(if (doc.breaks.contains(i+1)) "#" else " ")+" "+ws.categoryValue(i+1)+":"+zs.intValue(i+1)+"\t  "+doc.ws.categoryValues.mkString(" "))
      if (zs.intValue(i) == prevzi && !doc.breaks.contains(i)) sb.append("_")
      else if (sb.length > 0) addThenReset(sb.toString)
      sb.append(ws.categoryValue(i))
      prevzi = zs.intValue(i)
    }
    if (sb.length > 0) addThenReset(sb.toString)
  }
  
  def ++=(docs:Iterable[Doc]): this.type = {
    //println("TopicPhraseCounts docs.length="+docs.size)
    docs.foreach(+=(_))
    //forIndex(numTopics)({i => println("topic %d counts=%d".format(i, counts(i).countsTotal))})
    //println("lengths median="+lengths.sorted.apply(lengths.length/2))
    //println("lengths mean="+(lengths.sum * 1.0 / lengths.length))
    this
  }
  
  def topicEntries(zi:Int, n:Int = 10): Seq[TopEntry[String]] = new TopN(n, counts(zi).counts.asDoubleSeq, counts(zi).categories)
  def topicPhrases(zi:Int, n:Int = 10, includeCounts:Boolean = true): Seq[String] = topicEntries(zi, n).map(e => if (includeCounts) e.category+":"+e.score.toInt else e.category)
  def topicPhrasesSummary(topicIndex:Int, n:Int = 10): String = "Topic "+topicIndex+"  "+ topicPhrases(topicIndex, n).mkString(" ")
  def topicsPhrasesSummary(n:Int = 10): String = Range(0, numTopics).map(topicPhrasesSummary(_, n)).mkString("\n")
}