package cc.factorie.app.nlp.mention

import java.io.{File, FileOutputStream, FileInputStream}
import cc.factorie.app.nlp._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import cc.factorie.app.nlp.phrase.{ChunkerOpts, CRFChunker}
import cc.factorie.app.nlp.load.{ChunkTag, BILOU2LayerChunkTag, BILOUChunkTag}
import cc.factorie.app.nlp.ner.{ConllChainNer, NerTag}



/**
 * User: cellier
 * Date: 10/28/13
 * Time: 11:24 PM
 * Finite State machine implementation to grab NP spans
 */

class ChunkBasedMentionList extends MentionList
/*
 * Object to retrieve Multilayered BILOU Tags
 */
object MultiLayerNPChunkMentionFinder extends NPChunkMentionFinder[BILOU2LayerChunkTag]{
  //Splits tag value and calls to retrieve NPs for the inner tags and outer tags
  override def getMentionSpans(document: Document): Seq[TokenSpan] ={
    val mentionSpans = ArrayBuffer[TokenSpan]()
    document.sentences.foreach{s=>
      val chunkTags = s.tokens.map(t => t.attr[BILOU2LayerChunkTag].categoryValue.split(":").map(layer => t -> layer)).map(layer => (layer(0),layer(1)))
      val (innerTags,outerTags) = chunkTags.unzip
      mentionSpans ++= getNPChunkSpans(s,innerTags)
      mentionSpans ++= getNPChunkSpans(s,outerTags)
    }
    mentionSpans.seq
  }
}
object NPChunkMentionFinder extends NPChunkMentionFinder[BILOUChunkTag]

class NPChunkMentionFinder[L<:ChunkTag](implicit m: Manifest[L]) extends DocumentAnnotator {
  def prereqAttrs = Seq(classOf[Token], classOf[Sentence],m.runtimeClass)
  def postAttrs = Seq(classOf[MentionList], classOf[MentionEntityType])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+ m.attr[MentionEntityType].categoryValue +":" +m.indexOf(token)).mkString(","); case _ => "_" }

  val upperCase = "[A-Z]+".r

  def process(document: Document) = {
    val mentions = addChunkMentions(document)
    document.attr += new MentionList() ++= mentions.sortBy(m => (m.head.stringStart, m.length))
    document
  }

  def addChunkMentions(document: Document): Seq[Mention] = {
    getMentionSpans(document).map{labelSpan =>
      val s = labelSpan
      val m = new Mention(s, s.length-1)
      //m.attr += new MentionType(m, "NAM")
      m.attr += new MentionEntityType(m,"")
      m
    }
  }

  def getMentionSpans(document: Document): Seq[TokenSpan] ={
    val mentionSpans = ArrayBuffer[TokenSpan]()
    document.sentences.foreach{s=>
      val chunkTags = s.tokens.map(t => t-> t.attr[BILOUChunkTag].categoryValue)
      mentionSpans ++= getNPChunkSpans(s,chunkTags)
    }
    mentionSpans.seq
  }

  def getNPChunkSpans(s: Sentence,chunkTags: IndexedSeq[(Token, String)]):Seq[TokenSpan]={
    val spans = ArrayBuffer[TokenSpan]()
    chunkTags.map{case (t,chunk) =>
      if (chunk != "O") {
        if(chunk == "U-NP") spans += new TokenSpan(s.section, t.positionInSection, 1)
        else if(chunk == "B-NP"){
          if(t.hasNext) {
            var lookFor = t.next
            while (lookFor.hasNext && lookFor.sentence == t.sentence && chunkTags(chunkTags.map(_._1.string).indexOf(lookFor.string))._2.matches("(I|L)-NP")) lookFor = lookFor.next
            spans += new TokenSpan(s.section, t.positionInSection, lookFor.positionInSection - t.positionInSection)
          } else  spans += new TokenSpan(s.section, t.positionInSection, 1)
        }
      }
    }
    spans.toSeq
  }

}



