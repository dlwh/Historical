package dlwh.cognates

import Types._
import Bipartite._
import scalanlp.optimize.KuhnMunkres;
import scalanlp.concurrent.ParallelOps._
import scalanlp.config.Configuration
import java.io.File
import scalanlp.stats.sampling.Rand
import scalanlp.util.Log
import scalanlp.util.Log._
import scalanlp.util.ConsoleLogging


/**
 * 
 * @author dlwh
 */

class GlossBipartite[F<:Factors](alphabet: Set[Char],
                         val tree: Tree,
                         cognates: Seq[Cognate],
                         languages: Seq[Language],
                         treePenalty: Double,
                         initDeathProb: Double,
                         allowSplitting: Boolean,
                         factorBroker: FactorBroker[F]) extends ConsoleLogging {
  def treeScore(s: State[F], io: InsideOutside[F]):Double = (
    io.treePrior(s.deathScores)
  )



  def initialState(words :Seq[Cognate], factors: F): State[F] = {
    // each word is in its own group
    val groups = words.map(c => Map(c.language -> c) )
    State(groups.toIndexedSeq, factors, Map.empty.withDefaultValue(initDeathProb));
  }

  final def calculateAffinities(s: State[F], group: Map[Language,Cognate], language: Language, words: Seq[Cognate]) = {
    val io = s.makeIO(alphabet, tree, group)
    val marg = io.marginalFor(language).get;
    val prior = treeScore(s,io.include(Cognate(language,"<dummy>")));
    assert(!prior.isNaN,"tree score went off the deep end");

    val aff = new Array[Double](words.length);
    for ( i <- 0 until words.length ) {
      if(!group.isEmpty && group.head._2.gloss != words(i).gloss) {
        aff(i) = Double.PositiveInfinity;
      } else {
        val likelihood = marg(words(i).word)
        println(group + " attach " + words(i).word + " " + likelihood );
        aff(i) = -(likelihood + prior);
      }
      assert(!aff(i).isNaN);
    }
    aff
  }

  final def doMatching(affinities: Array[Array[Double]]):IndexedSeq[Int] = {
    if(affinities.size == 0) IndexedSeq[Int]();
    else {
      val bm = KuhnMunkres;
      val size = affinities.size max affinities(0).size;
      val paddedAffinities = Array.tabulate(size,size) { (x,y) =>
        if (x < affinities.length && y < affinities(x).length) affinities(x)(y)
        else 0.0;
      };
      val changes = bm.extractMatching(paddedAffinities.map(_.toIndexedSeq).toIndexedSeq)._1;
      changes.take(affinities.length);
    }
  }

  // score to keep a cognate separate
  def baselineScores(s: State[F], language: Language, words: Seq[Cognate]) = {
    val arr = calculateAffinities(s,Map.empty,language,words);
    for( i <- 0 until arr.length) {
      arr(i) += treePenalty;
    }
    arr
  }

  def buildGroups(s: State[F],
                  l: Language,
                  words: IndexedSeq[Cognate],
                  groups: IndexedSeq[Map[Language,Cognate]]):
                 (Double, IndexedSeq[Map[Types.Language, Cognate]]) = {
    val affinities: Array[Array[Double]] = groups.par.map { group =>
      calculateAffinities(s,group,l, words);
    } toArray;

    println(words.size,groups.size);
    println(affinities.size);
    val assignments = doMatching(affinities).toArray;
    val baselines = baselineScores(s,l,words);
    // Compute the score, and patch anything that is lower score than just leaving it by itself.
    var score = 0.0
    var loners = Set[Cognate]() ++ words;
    for( (w,g) <- assignments.zipWithIndex if w != -1 && w < words.length) {
      val affScore = affinities(g)(w);
      // i.e. if -log prob of recommended attachment is worse than going it alone, just go it alone
      log(INFO)(words(w) + " " +  affScore + " " + baselines(w) + groups(g));
      if(affScore <= baselines(w) || !allowSplitting) {
        log(INFO)("attach! " + words(w) + " " + groups(g));
        score += -affinities(g)(w);
        loners -= words(w);
      } else {
        log(INFO)("go it alone! " + words(w) + " " + groups(g));
        assignments(g) = -1;
        score += -baselines(w);
      }
    }

    val augmentedGroups: IndexedSeq[Map[Types.Language, Cognate]] = for {
      (group,g) <- groups.zipWithIndex;
      newWordIndex = assignments(g)
    } yield {
      if(newWordIndex != -1 && newWordIndex < words.length) group + (l-> words(newWordIndex))
      else group;
    }
    val newGroups = augmentedGroups ++ loners.map(c => Map(l-> c));

    (score,newGroups);

  }

  def step(s: State[F], language: Language):State[F] = {
    val wordsInCurrentLanguage = cognates.filter(_.language == language).groupBy(_.gloss);
    val otherLanguages = s.groups.map(group => group - language).filterNot(_.isEmpty)
    val otherLanguagesByGloss = otherLanguages.groupBy(m => m.iterator.next._2.gloss);

    log(INFO)("??? " + language + wordsInCurrentLanguage);
    val regrouped: IndexedSeq[(Double, scala.IndexedSeq[Map[Types.Language, Cognate]])] = (for((gloss,curWords) <- wordsInCurrentLanguage.iterator) yield {
      val groupsToMatch = otherLanguagesByGloss.getOrElse(gloss, IndexedSeq.empty)
      log(INFO)(gloss,curWords,groupsToMatch);
      val r = buildGroups(s, language, curWords toIndexedSeq, groupsToMatch);
      if(groupsToMatch.isEmpty) log(INFO)(gloss + " empty!")
      else log(INFO)("Result: " + gloss + " " + r._2);
      r
    }).toIndexedSeq

    val glossesNotInLanguage = otherLanguagesByGloss.filterKeys(gloss => !wordsInCurrentLanguage.keySet(gloss));
    val otherGroups = glossesNotInLanguage.values.flatten;
    val score = regrouped.iterator.map(_._1).sum;
    val flattened: IndexedSeq[Map[Types.Language, Cognate]] = regrouped.map(_._2).flatten;

    val newGroups = flattened ++ otherGroups.toIndexedSeq;

    val newKnownLanguages = s.knownLanguages + language;
    val deathProbs = if(newKnownLanguages.size >= languages.size) learnDeathProbs(s,newGroups) else s.deathScores;
    log(INFO)(newGroups);
    log(INFO)(deathProbs);
    val newS:State[F] = s.copy(groups = newGroups,
      likelihood = score,
      deathScores = deathProbs
    )
    log(INFO)("about to hp into ios...");
    val nextIOs = newS.groups.iterator.map(newS.makeIO(alphabet,tree,_));
    log(INFO)("about to hp into nextFactors...");
    val newFactors = factorBroker.nextFactors(nextIOs, newS);
    newS.copy(factors = newFactors);
  }

  def learnDeathProbs(s: State[F], groups: Seq[Map[Language,Cognate]]) = {
    val deathCounts = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    val availableCounts = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    for {
      group <- groups;
      io = s.makeIO(alphabet,tree,group);
      (lPair,lastOnPath) <- io.pathsToFringe
    } {
      availableCounts(lPair) += 1;
      if(lastOnPath) deathCounts(lPair) += 1;
    }

    val normalized = for {
      (pair,total) <- availableCounts
      deaths = deathCounts(pair)
    } yield (pair,(deaths + initDeathProb) / (total + 1.0));

    normalized.toMap.withDefaultValue(initDeathProb);
  }

  def iterations = {
    val state = initialState(cognates.filter(_.language == languages(0)),factorBroker.initialFactors);
    val lang: Iterator[Types.Language] = Iterator.continually { languages.iterator.drop(1) ++ Iterator.single(languages(0)) } flatten;
    scanLeft(lang,state)(step(_,_));
  }
}

object GlossBipartiteRunner {
  import Accuracy._;

  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val languages = config.readIn[String]("dataset.languages").split(",");
    val withGloss = config.readIn[Boolean]("dataset.hasGloss",false);
    println(withGloss);

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages, withGloss);
    val tree = dataset.tree;
    val leaves = tree.leaves;

    val gold = dataset.cognates.map(_.filter(cog => leaves(cog.language)));
    val goldIndices = indexGold(gold);

    val data = gold.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val alphabet = Set.empty ++ data.iterator.flatMap(_.word.iterator);

    val Params(treePenalty,initDeathProb,allowDeath) = config.readIn[Params]("params");
    val messageCompression = readAutomataCompressor(config, "transducers.message");

    val transducerCompression = config.readIn[String]("transducers.editdistance","uni") match {
      case "unigram" | "uni" =>
        new UniCompression(('#','#')) with NormalizedByFirstChar[Unit,Char];
    }

    globalLog.log(INFO)("post aff pregc " + memoryString);
    System.gc();
    globalLog.log(INFO)("post aff postgc " + memoryString);
    //val rootCompression = readAutomataCompressor(config, "transducers.root");

    val shouldLearn = config.readIn[Boolean]("transducers.learning");
    val initialFactors = new TransducerFactors(tree,alphabet,messageCompression);
    val broker = (
      if(!shouldLearn) new ConstantBroker(initialFactors)
      else new TransducerBroker(tree, alphabet, initialFactors, messageCompression, transducerCompression)
    );

    globalLog.log(INFO)("post aff pregc " + memoryString);
    System.gc();
    globalLog.log(INFO)("post aff postgc " + memoryString);

    val bipartite = new GlossBipartite[TransducerFactors](alphabet, tree, randomized, languages, treePenalty, initDeathProb, allowDeath, broker);
    val iter = bipartite.iterations;
    val numPositives = numberOfPositives(languages, gold);

    Log.globalLog(INFO)("Here");
    for( state <- iter.take(1000)) {
      val numGroups = state.groups.length;
      for(g <- 0 until numGroups) {
        val cognates = state.groups(g);
        val indices = cognates.valuesIterator map goldIndices toSeq;
        println(cognates.mkString(",") + " " + indices.mkString(","));
      }
      println("Conditional Likelihood" + state.likelihood)
      val groundedGroups = state.groups map { group => group.mapValues(goldIndices).toMap };
      val (precisions,recalls) = precisionAndRecall(languages,groundedGroups,numPositives);
      println("Precisions" + precisions);
      println("Recall" + recalls);
    }
  }

  case class MessageParams(`type`: String, klThreshold: Double, maxStates: Int);

  private def readAutomataCompressor(config: Configuration, prefix: String): MessageCompressor[_] = {
    val MessageParams(tpe, klThreshold, maxStates) = config.readIn[MessageParams](prefix);
    val beginningUnigram = '#';
    tpe match {
      case "bigram" | "bi" =>
        new BiCompression(klThreshold,maxStates,beginningUnigram) with NormalizedTransitions[Option[Char],Char] : MessageCompressor[_];
      case "unigram" | "uni" =>
        new UniCompression(beginningUnigram) with NormalizedTransitions[Unit,Char] : MessageCompressor[_];
      case "posunigram" | "posuni" =>
        new PosUniCompression(maxStates,beginningUnigram) with NormalizedTransitions[Int,Char] : MessageCompressor[_];
    }
  }
}