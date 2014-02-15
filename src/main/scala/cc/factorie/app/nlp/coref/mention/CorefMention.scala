package cc.factorie.app.nlp.coref.mention

import cc.factorie.app.nlp.coref._
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.{Section, Token, TokenSpan}
import cc.factorie.app.strings.Stopwords
import scala.collection.mutable
import cc.factorie.app.nlp.phrase.{Phrase, NumberLabel, GenderLabel}
import cc.factorie.util.Attr
import cc.factorie.variable.Span

/**
 * User: apassos
 * CorefMention: Wrapper on Mention
 * Update Feb '14: Cellier
 *    Removed Mention, Made CorefMention extend from Phrase
 */

object Mention{
  def phraseToMention(p: Phrase): Mention = {
    val headOffset = if(p.headTokenOffset == -1) getHeadIdx(p.value) else p.headTokenOffset
    val cm = new Mention(p.section,p.start,p.length,headOffset)
    cm.attr += new MentionEntityType(cm, cm.attr[MentionEntityType].categoryValue)
    cm
  }

  def getHeadIdx(s: Span[Section,Token], mentionType: String = null): Int = {
    val idx = s.lastIndexWhere(_.posTag.categoryValue.startsWith("NN"))
    //assert(idx != -1, "failed to find a noun in the span") //todo: put this back in
    if(idx < 0)  s.length -1 else idx      //todo: this handles the case where it didn't find an NN anywhere. Should that be happening?
  }

  //todo: the default headTokenIndex here is a little funky. If you don't pass it anything, it sets it using the NN heuristic below.
  //todo: however, if the span truly has the root as its parse parent, we also use the NN heuristic. Is there something else we should be doing for this second case?
  def apply(span: TokenSpan, tokenNum: Int, sentenceNum: Int, mentionType: String = null, headTokenIndex: Int = -1) = {
    //here, we use the final noun as the head if it wasn't passed a headTokenIndex (from parsing)
    val headInd = {
      if(headTokenIndex > span.length){
        throw new IllegalStateException("the constructor expects headTokenIndex to be an offset from the beginning of the span, not the document")
      }
      if(headTokenIndex == -1){
        val idx = span.value.lastIndexWhere(_.posTag.categoryValue.startsWith("NN"))
        //assert(idx != -1, "failed to find a noun in the span") //todo: put this back in
        if(idx < 0)  span.length -1 else idx      //todo: this handles the case where it didn't find an NN anywhere. Should that be happening?
      }else
        headTokenIndex
    }
    val mention = new Mention(span.section, tokenNum,  sentenceNum, headInd)
    mention.attr += new MentionType(mention,mentionType)
  }

  val posTagsSet = Set("PRP", "PRP$", "WP", "WP$")

  val properSet = Set("NNP", "NNPS")

  val nounSet = Seq("NN", "NNS")

  val posSet = Seq("POS")
}

/** A Phrase holding a mention of an entity.
    Note that headTokenIndex is an offset from the beginning of this span, not the beginning of the Section.
    Note also that since Mention is a Span, and Span is a sequence over Tokens, "this.head" is the first token of the span, not the "natural language head" of the phrase; for the later use "this.headToken". */
class Mention(section:Section, start:Int, length:Int, val headTokenIndex: Int = -1) extends Phrase(section,start,length,headTokenIndex) with Attr {
  def this(phrase:Phrase) = this(phrase.section, phrase.start, phrase.length, phrase.headTokenOffset)
  //val _head =  mention.tokens(mention.headTokenIndex)  //here, the head token index is an offset into the span, not the document
  val _head = tokens(headTokenIndex)
  //def headToken: Token = _head
  def parentEntity = this.attr[Entity]
  def mType = headToken.posTag.categoryValue
  //def span = mention
  def entityType: String = this.attr[MentionEntityType].categoryValue
  //def document = section.document

  val isPRO = Mention.posTagsSet.contains(mType)
  val isProper = Mention.properSet.contains(this.headToken.posTag.categoryValue)
  val isNoun = Mention.nounSet.contains(this.headToken.posTag.categoryValue)
  val isPossessive = Mention.posSet.contains(this.headToken.posTag.categoryValue)

  def isAppositionOf(m : Mention) : Boolean = {
    val isAppo = headToken.parseLabel.categoryValue == "appos"
    val isChildOf = headToken.parseParent == m.headToken
    isAppo && isChildOf
  }

  var cache = new MentionCache(this)
  def clearCache() {
    cache = new MentionCache(this)
  }

  def hasSpeakWord: Boolean = cache.hasSpeakWord
  def gender = cache.gender
  def number = cache.number
  def nonDeterminerWords: Seq[String] = cache.nonDeterminerWords
  def acronym = cache.acronym
  def nounWords = cache.nounWords
  //def acronym: Set[String] = cache.acronym
  //def nounWords: Set[String] = cache.nounWords
  def lowerCaseHead: String = cache.lowerCaseHead
  def initials: String = cache.initials
  def predictEntityType: String = cache.predictEntityType
  def headPhraseTrim: String = cache.headPhraseTrim
  def demonym: String = cache.demonym
  def capitalization: Char = cache.capitalization
  def wnLemma: String = cache.wnLemma
  def wnSynsets = cache.wnSynsets
  def wnHypernyms = cache.wnHypernyms
  def wnAntonyms = cache.wnAntonyms

  def printInfo : String = Seq[String]("gender", gender,"number", number,"nondet",nonDeterminerWords.mkString(" "),"acronym",acronym.mkString(" "),"nounwords",nounWords.mkString(" "),"lowercasehead",lowerCaseHead,"initials",initials,"ent-type",predictEntityType,"head-phase-trim",headPhraseTrim,"capitalization",capitalization.toString,"wnlemma",wnLemma).mkString("\n")
}

class MentionCache(m: Mention) {
  import cc.factorie.app.nlp.lexicon
  lazy val hasSpeakWord = m.exists(s => lexicon.iesl.Say.contains(s.string))
  lazy val wnLemma = WordNet.lemma(m.headToken.string, "n")
  lazy val wnSynsets = WordNet.synsets(wnLemma).toSet
  lazy val wnHypernyms = WordNet.hypernyms(wnLemma)
  lazy val wnAntonyms = wnSynsets.flatMap(_.antonyms()).toSet
  lazy val nounWords: Set[String] =
      m.tokens.filter(_.posTag.categoryValue.startsWith("N")).map(t => t.string.toLowerCase).toSet
  lazy val lowerCaseHead: String = m.phrase.toLowerCase
  lazy val headPhraseTrim: String = m.phrase.trim
  lazy val nonDeterminerWords: Seq[String] =
    m.tokens.filterNot(_.posTag.categoryValue == "DT").map(t => t.string.toLowerCase)
  lazy val initials: String =
      m.tokens.map(_.string).filterNot(lexicon.iesl.OrgSuffix.contains).filter(t => t(0).isUpper).map(_(0)).mkString("")
  lazy val predictEntityType: String = m.attr[MentionEntityType].categoryValue
  lazy val demonym: String = lexicon.iesl.DemonymMap.getOrElse(m.headPhraseTrim, "")

  lazy val capitalization: Char = {
      if (m.length == 1 && m.head.positionInSentence == 0) 'u' // mention is the first word in sentence
          val s = m.value.filter(_.posTag.categoryValue.startsWith("N")).map(_.string.trim)
          if (s.forall(_.forall(_.isUpper))) 'a'
          else if (s.forall(t => t.head.isLetter && t.head.isUpper)) 't'
          else 'f'
    }
  lazy val gender = m.attr[GenderLabel].intValue.toString
  lazy val number = m.attr[NumberLabel].intValue.toString
  lazy val acronym: Set[String] = {
    if (m.length == 1)
        Set.empty
      else {
        val alt1 = m.value.map(_.string.trim).filter(_.exists(_.isLetter)) // tokens that have at least one letter character
        val alt2 = alt1.filterNot(t => Stopwords.contains(t.toLowerCase)) // alt1 tokens excluding stop words
        val alt3 = alt1.filter(_.head.isUpper) // alt1 tokens that are capitalized
        val alt4 = alt2.filter(_.head.isUpper)
        Seq(alt1, alt2, alt3, alt4).map(_.map(_.head).mkString.toLowerCase).toSet
      }
  }
}

object CorefFeatures {
  def getPairRelations(s1: Mention, s2: Mention): String = {
    val l1 = s1.headToken.string.toLowerCase
    val l2 = s2.headToken.string.toLowerCase
    if (l1 == l2)
      "match"
    else if (l1.contains(l2) || l2.contains(l1))
      "substring"
    else if (s1.wnSynsets.exists(a => s2.wnSynsets.contains(a)))
      "Syn"
    else if (s1.wnSynsets.exists(a => s2.wnHypernyms.contains(a)) || s2.wnSynsets.exists(a => s1.wnHypernyms.contains(a)))
      "Hyp"
    else if (s1.wnSynsets.exists(s2.wnAntonyms.contains))
      "Ant"
    else
      "Mismatch"
  }

  def matchingTokensRelations(m1: Mention, m2: Mention) = {
    import cc.factorie.app.nlp.lexicon
    val set = new mutable.HashSet[String]()
    for (w1 <- m2.toSeq.map(_.string.toLowerCase))
      for (w2 <- m1.toSeq.map(_.string.toLowerCase))
       if (w1.equals(w2) || m2.wnSynsets.exists(m1.wnHypernyms.contains) || m1.wnHypernyms.exists(m2.wnHypernyms.contains) ||
           lexicon.iesl.Country.contains(w1) && lexicon.iesl.Country.contains(w2) ||
           lexicon.iesl.City.contains(w1) && lexicon.iesl.City.contains(w2) ||
           lexicon.uscensus.PersonFirstMale.contains(w1) && lexicon.uscensus.PersonFirstMale.contains(w2) ||
           // commented out the femaleFirstNames part, Roth publication did not use
           lexicon.uscensus.PersonFirstFemale.contains(w1) && lexicon.uscensus.PersonFirstFemale.contains(w2) ||
           lexicon.uscensus.PersonLast.contains(w1) && lexicon.uscensus.PersonLast.contains(w2))
        set += getPairRelations(m1, m2)
    set.toSet
  }

  def countCompatibleMentionsBetween(m1: Mention, m2: Mention, mentions: Seq[Mention]): Seq[String] = {
    val doc = m1.document
    val ments = mentions.filter(m => m.start < m1.start && m.start > m2.end)
    val iter = ments.iterator
    var numMatches = 0
    while (numMatches <= 2 && iter.hasNext) {
      val m = iter.next()
      if (CorefFeatures.gendersMatch(m, m1).equals("t") && CorefFeatures.numbersMatch(m, m1).equals("t")) numMatches += 1
    }

    if (numMatches <= 2) (0 to numMatches).map(_.toString)
    else (0 to numMatches).map(_.toString) :+ "_OVER2"
  }

  val maleHonors = Set("mr", "mister")
  val femaleHonors = Set("ms", "mrs", "miss", "misses")
  val neuterWN = Set("artifact", "location", "group")

  val malePron = Set("he", "him", "his", "himself")
  val femalePron = Set("she", "her", "hers", "herself")
  val neuterPron = Set("it", "its", "itself", "this", "that", "anything", "something",  "everything", "nothing", "which", "what", "whatever", "whichever")
  val personPron = Set("you", "your", "yours", "i", "me", "my", "mine", "we", "our", "ours", "us", "myself", "ourselves", "themselves", "themself", "ourself", "oneself", "who", "whom", "whose", "whoever", "whomever", "anyone", "anybody", "someone", "somebody", "everyone", "everybody", "nobody")

  val allPronouns = maleHonors ++ femaleHonors ++ neuterWN ++ malePron ++ femalePron ++ neuterPron ++ personPron
  // TODO: this cache is not thread safe if we start making GenderMatch not local
  // val cache = scala.collection.mutable.Map[String, Char]()
  import cc.factorie.app.nlp.lexicon
  def namGender(m: Mention): Char = {
    val fullhead = m.phrase.trim.toLowerCase
    var g = 'u'
    val words = fullhead.split("\\s")
    if (words.length == 0) return g

    val word0 = words.head
    val lastWord = words.last

    var firstName = ""
    var honor = ""
    if (lexicon.iesl.PersonHonorific.contains(word0)) {
      honor = word0
      honor = removePunct(honor)
      if (words.length >= 3)
        firstName = words(1)
    } else if (words.length >= 2) {
      firstName = word0
    } else {
      firstName = word0
    }

    // determine gender using honorifics
    if (maleHonors.contains(honor))
      return 'm'
    else if (femaleHonors.contains(honor))
      return 'f'

    // determine from first name
    if (lexicon.uscensus.PersonFirstMale.contains(firstName))
      g = 'm'
    else if (lexicon.uscensus.PersonFirstFemale.contains(firstName))
      g = 'f'
    else if (lexicon.uscensus.PersonLast.contains(lastWord))
      g = 'p'

    if (lexicon.iesl.City.contains(fullhead) || lexicon.iesl.Country.contains(fullhead)) {
      if (g.equals("m") || g.equals("f") || g.equals("p"))
        return 'u'
      g = 'n'
    }

    if (lexicon.iesl.OrgSuffix.contains(lastWord)) {
      if (g.equals("m") || g.equals("f") || g.equals("p"))
        return 'u'
      g = 'n'
    }

    g
  }

  def nomGender(m: Mention, wn: WordNet): Char = {
    val fullhead = m.phrase.toLowerCase
    if (wn.isHypernymOf("male", fullhead))
      'm'
    else if (wn.isHypernymOf("female", fullhead))
      'f'
    else if (wn.isHypernymOf("person", fullhead))
      'p'
    else if (neuterWN.exists(wn.isHypernymOf(_, fullhead)))
      'n'
    else
      'u'
  }


  def proGender(m: Mention): Char = {
    val pronoun = m.phrase.toLowerCase
    if (malePron.contains(pronoun))
      'm'
    else if (femalePron.contains(pronoun))
      'f'
    else if (neuterPron.contains(pronoun))
      'n'
    else if (personPron.contains(pronoun))
      'p'
    else
      'u'
  }


  def strongerOf(g1: Char, g2: Char): Char = {
    if ((g1 == 'm' || g1 == 'f') && (g2 == 'p' || g2 == 'u'))
      g1
    else if ((g2 == 'm' || g2 == 'f') && (g1 == 'p' || g1 == 'u'))
      g2
    else if ((g1 == 'n' || g1 == 'p') && g2 == 'u')
      g1
    else if ((g2 == 'n' || g2 == 'p') && g1 == 'u')
      g2
    else
      g2
  }

  def gendersMatch(m1: Mention, m2: Mention): Char = {
    val g1 = m2.gender
    val g2 = m1.gender

    if (g1 == 'u' || g2 == 'u')
      'u'
    else if (g1 == 'p' && (g2 == 'm' || g2 == 'f' || g2 == 'p'))
      'u'
    else if (g2 == 'p' && (g1 == 'm' || g1 == 'f' || g1 == 'p'))
      'u'
    else if (g1 == g2)
      't'
    else
      'f'
  }

  def headWordsCross(m1: Mention, m2: Mention, model: PairwiseCorefModel): String = {
    val w1 = m2.headPhraseTrim
    val w2 = m1.headPhraseTrim
    val rare1 = 1.0 / model.MentionPairLabelThing.tokFreq.getOrElse(w1.toLowerCase, 1).toFloat > 0.1
    val rare2 = 1.0 / model.MentionPairLabelThing.tokFreq.getOrElse(w2.toLowerCase, 1).toFloat > 0.1
    if (rare1 && rare2 && w1.equalsIgnoreCase(w2))
      "Rare_Duplicate"
    else
      (if (rare1) "RARE" else w1) + "_AND_" + (if (rare2) "RARE" else w2)
  }

  val singPron = Set("i", "me", "my", "mine", "myself", "he", "she", "it", "him", "her", "his", "hers", "its", "one", "ones", "oneself", "this", "that")
  val pluPron = Set("we", "us", "our", "ours", "ourselves", "ourself", "they", "them", "their", "theirs", "themselves", "themself", "these", "those")
  val singDet = Set("a ", "an ", "this ")
  val pluDet = Set("those ", "these ", "some ")

  def numbersMatch(m1: Mention, m2: Mention): Char = {
    val n1 = m2.number
    val n2 = m1.number

    if (n1 == n2 && n1 != 'u')
      't'
    else if (n1 != n2 && n1 != 'u' && n2 != 'u')
      'f'
    else if (n1 == 'u' || n2 == 'u') {
      if (m1.toSeq.map(t => t.string.trim).mkString(" ").equals(m2.toSeq.map(t => t.string.trim).mkString(" ")))
        't'
      else
        'u'
    }
    else
      'u'
  }

  val relativizers = Set("who", "whom", "which", "whose", "whoever", "whomever", "whatever", "whichever", "that")

  def areAppositive(m1: Mention, m2: Mention): Boolean = {
    (m2.isProper || m1.isProper) &&
      (m2.last.next(2) == m1.head && m2.last.next.string.equals(",") ||
        m1.last.next(2) == m2.head && m1.last.next.string.equals(","))
  }

  def isRelativeFor(m1: Mention, m2: Mention) =
    relativizers.contains(m1.lowerCaseHead) &&
      (m2.head == m1.last.next ||
        (m2.head == m1.last.next(2) && m1.last.next.string.equals(",")
          || m2.head == m1.last.next(2) && m1.last.next.string.equals(",")))


  def areRelative(m1: Mention, m2: Mention): Boolean = isRelativeFor(m1, m2) || isRelativeFor(m2, m1)

  def canBeAliases(m1: Mention, m2: Mention): Boolean = {
    val eType1 = m2.predictEntityType
    val eType2 = m1.predictEntityType

   // val m1head = m2.span
   // val m2head = m1.span
    val m1Words = m1.phrase.split("\\s")
    val m2Words = m2.phrase.split("\\s")

    if (m2.isProper && m1.isProper && m2.predictEntityType.equals(m1.predictEntityType) && (m2.predictEntityType.equals("PERSON") || m2.predictEntityType.equals("GPE")))
      return m2.last.string.toLowerCase equals m1.last.string.toLowerCase

    else if ((eType1.equals("ORG") || eType1.equals("unknown")) && (eType2.equals("ORG") || eType2.equals("unknown"))) {
      val (initials, shorter) =
        if (m1Words.length < m2Words.length)
          (m2.initials, m1.phrase)
        else
          (m1.initials, m2.phrase)
      return shorter.replaceAll("[., ]", "") equalsIgnoreCase initials
    }

    false
  }


  lazy val punct = "^['\"(),;.`]*(.*?)['\"(),;.`]*$".r
  def removePunct(s: String): String = {
    val punct(ret) = s
    ret
  }

}
