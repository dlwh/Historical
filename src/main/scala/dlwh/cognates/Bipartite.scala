package dlwh.cognates

import scalanlp.fst._
import scalanlp.stats.sampling.Rand;
import scala.actors.Future
import scala.collection.mutable.ArrayBuffer
import java.io.FileInputStream
import java.io.File
import java.util.Properties
import scalanlp.config._;
import scalala.Scalala._;
import scalala.tensor.counters.LogCounters._;
import scalala.tensor.counters.Counters;
import Types._;
import scalanlp.util.Index
import scalanlp.util.Log._;
import fig.basic.BipartiteMatcher;

object Bipartite {

  trait FactorBroker[F<:Factors] {
    def initialFactors: F
    def nextFactors(ios: Iterator[InsideOutside[F]], state: State[F]): F;
  }

  class ConstantBroker[F<:Factors](val initialFactors: F) extends FactorBroker[F] {
    def nextFactors(ios: Iterator[InsideOutside[F]], state: State[F]) = initialFactors;
  }

  class TransducerBroker(val tree: Tree,
                         val alphabet: Set[Char],
                         val initialFactors: TransducerFactors,
                         val messageCompression: MessageCompressor[_],
                         val transducerCompressor: Compressor[_,(Char,Char)],
                         val rootCompressor: Compressor[_,Char])
                         extends FactorBroker[TransducerFactors] with TransducerLearning {

    override def nextFactors(ios: Iterator[InsideOutside[TransducerFactors]], s: State[TransducerFactors]) = {
      val (stats,root) = gatherStatistics(ios);
      val newFactors = mkFactors(stats,root);
      newFactors
    }

    def mkFactors(statistics: Statistics,root:RootStats):TransducerFactors = {
      val transducers = mkTransducers(statistics);
      val rootM = mkRoot(root);
      val factors = new TransducerFactors(tree,alphabet,messageCompression,transducers,root=Some(rootM)) ;
      globalLog.log(INFO)("Trans out " + memoryString);
      factors
    }
  }


  case class State[F](groups: Seq[Map[Language,Cognate]],
                      factors: F,
                      deathScores:Map[(Language,Language),Double],
                      likelihood:Double = Double.NegativeInfinity,
                      knownLanguages: Set[Language] = Set.empty);
}


import Bipartite._;

class Bipartite[F<:Factors](val tree: Tree,
                         cognates: Seq[Cognate],
                         languages: Seq[Language],
                         treePenalty: Double,
                         initDeathProb: Double,
                         allowSplitting: Boolean,
                         factorBroker: FactorBroker[F]) {

  def makeIO(s:State[F], words: Map[Language,Cognate]) = {
    println("Looking at " + words);
    val io = new InsideOutside(tree, s.factors, Map.empty ++ words.mapValues(_.word));
    io;
  }

  def treeScore(s: State[F], io: InsideOutside[F]):Double = (
    io.treePrior(s.deathScores)
  )


  def initialState(words :Seq[Cognate], factors: F): State[F] = {
    // the n'th word in each language is assigned to the n'th cognate group
    val groupMap = words.groupBy(_.language).valuesIterator.flatMap(_.zipWithIndex.iterator).toSeq.groupBy(_._2);
    val groups = for(group  <- groupMap.valuesIterator) yield {
      val cogs = for( (cog,i) <- group) yield (cog.language,cog);
      Map.empty ++ cogs;
    };
    State(groups.toSeq, factors, Map.empty.withDefaultValue(initDeathProb), knownLanguages = words.iterator.map(_.language).toSeq.toSet);
  }

  final def calculateAffinities(s: State[F], group: Map[Language,Cognate], language: Language, words: Seq[Cognate]) = {
    val io = makeIO(s,group)
    val marg = io.marginalFor(language).get;
    //println(group,marg);
    val prior = treeScore(s,io.include(language,"<dummy>"));
    assert(!prior.isNaN)

    val aff = new Array[Double](words.length);
    for ( i <- 0 until words.length ) {
      aff(i) = (marg(words(i).word) + prior);
      //println(aff(i),words(i),group,prior);
      assert(!aff(i).isNaN);
    }
    aff
  }

  final def doMatching(affinities: Array[Array[Double]]) = {
    val bm = new BipartiteMatcher();
    val size = affinities.size max affinities(0).size;
    val paddedAffinities = Array.tabulate(size,size) { (x,y) =>
      if (x < affinities.length && y < affinities(x).length) affinities(x)(y)
      else 0.0;
    };
    val changes = bm.findMaxWeightAssignment(paddedAffinities);
    changes.take(affinities.length);
  }

  def baselineScores(s: State[F], language: Language, words: Seq[Cognate]) = {
    val arr = calculateAffinities(s,Map.empty,language,words);
    for( i <- 0 until arr.length) {
      arr(i) += treePenalty;
    }
    arr
  }

  final def step(s: State[F], language: Language):State[F] = {
    val current = cognates.filter(_.language == language);
    val otherLanguages = s.groups.map(group => group - language).filter(!_.isEmpty);

    println(language);

    val tasks = for ( j <- 0 until otherLanguages.length) yield { () =>
      val aff = calculateAffinities(s,otherLanguages(j),language,current)
      (j,aff)
    }

    val affinities = new Array[Array[Double]](otherLanguages.length);
    TaskExecutor.doTasks(tasks) foreach { case (j,aff) =>
      affinities(j) = aff;
    }


    val changes = doMatching(affinities);
    assert(changes.toSet.size == changes.length);

    // Calculate baseline scores for all words
    val baselines = baselineScores(s,language,current);
    // Compute the score, and patch anything that is lower score than just leaving it by itself.
    var score = 0.0
    var loners = Set[Cognate]();
    for( (w,g) <- changes.zipWithIndex if w != -1 && w < current.length) {
      val affScore = affinities(g)(w);
      // i.e. if -log prob of recommended attachment is worse than going it alone, just go it alone
      println(current(w) + " " +  affScore + " " + baselines(w) + otherLanguages(g));
      if(affScore >= baselines(w) || !allowSplitting) {
        score += affinities(g)(w);
      } else {
        changes(g) = -1;
        loners += current(w);
        score += baselines(w);
      }
    }

    val augmentedGroups = for {
      (group,g) <- otherLanguages.zipWithIndex;
      newWordIndex = changes(g)
    } yield {
      if(newWordIndex != -1 && newWordIndex < current.length) group + (language -> current(newWordIndex))
      else group;
    }

    val newGroups = augmentedGroups ++ loners.map(c => Map.empty + (language -> c));

    val newKnownLanguages = s.knownLanguages + language;
    val deathProbs = if(newKnownLanguages.size >= languages.size) learnDeathProbs(s,newGroups) else s.deathScores;
    println(deathProbs);

    val newS = s.copy(groups = newGroups,
                      likelihood = score,
                      deathScores = deathProbs,
                      knownLanguages = newKnownLanguages
                      );
    val nextIOs = newS.groups.iterator.map(makeIO(newS,_));
    val newFactors = factorBroker.nextFactors(nextIOs, newS);
    newS.copy(factors = newFactors);
  }

  def learnDeathProbs(s: State[F], groups: Seq[Map[Language,Cognate]]) = {
    val deathCounts = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    val availableCounts = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    for {
      group <- groups;
      io = makeIO(s,group);
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
    val lang= Iterator.continually { languages.iterator.drop(1) ++ Iterator.single(languages(0)) } flatten;
    scanLeft(lang,state)(step(_,_));
  }

  def scanLeft[A,B](it: Iterator[A], b: B)(f: (B,A)=>B):Iterator[B] = new Iterator[B] {
    def hasNext = it.hasNext;
    var c = b;
    def next = {
      val a = it.next;
      c = f(c,a);
      c;
    }
  }

}

abstract class BaselineX(val tree: Tree, cognates: Seq[Cognate], languages: Seq[Language], threshold: Double) {
  type MFactors <: Factors
  case class State(groups: Seq[Map[Language,Cognate]],
                   factors: MFactors,
                   likelihood:Double = Double.NegativeInfinity,
                   knownLanguages: Set[Language] = Set.empty);

  def makeIO(s:State, words: Map[Language,Cognate]) = {
    println("Looking at " + words);
    val io = new InsideOutside(tree, s.factors, Map.empty ++ words.mapValues(_.word));
    io;
  }

  def initialFactors: MFactors

  def initialState(words :Seq[Cognate], factors: MFactors): State = {
    // the n'th word in each language is assigned to the n'th cognate group
    val groupMap = words.groupBy(_.language).valuesIterator.flatMap(_.zipWithIndex.iterator).toSeq.groupBy(_._2);
    val groups = for(group  <- groupMap.valuesIterator) yield {
      val cogs = for( (cog,i) <- group) yield (cog.language,cog);
      Map.empty ++ cogs;
    };
    State(groups.toSeq, factors,knownLanguages = words.iterator.map(_.language).toSeq.toSet);
  }

  final def calculateAffinities(s: State, group: Map[Language,Cognate], language: Language, words: Seq[Cognate]) = {
    val io = makeIO(s,group)
    val marg = io.marginalFor(language).get;
    //println(group,marg);

    val aff = new Array[Double](words.length);
    for ( i <- 0 until words.length ) {
      aff(i) = (marg(words(i).word));
      //println(aff(i),words(i),group,prior);
      assert(!aff(i).isNaN);
    }
    aff
  }

  final def doMatching(affinities: Array[Array[Double]]) = {
    val bm = new BipartiteMatcher();
    val size = affinities.size max affinities(0).size;
    val paddedAffinities = Array.tabulate(size,size) { (x,y) =>
      if (x < affinities.length && y < affinities(x).length) affinities(x)(y)
      else 0.0;
    };
    val changes = bm.findMaxWeightAssignment(paddedAffinities);
    changes.take(affinities.length);
  }

  final def step(s: State, language: Language):State = {
    val current = cognates.filter(_.language == language);
    val otherLanguages = s.groups.map(group => group - language).filter(!_.isEmpty);

    val tasks = for ( j <- 0 until otherLanguages.length) yield { () =>
      val aff = calculateAffinities(s,otherLanguages(j),language,current)
      (j,aff)
    }

    val affinities = new Array[Array[Double]](otherLanguages.length);
    TaskExecutor.doTasks(tasks) foreach { case (j,aff) =>
      affinities(j) = aff;
    }


    val changes = doMatching(affinities);
    assert(changes.toSet.size == changes.length);

    // Compute the score, and patch anything that is lower score than just leaving it by itself.
    var score = 0.0
    var loners = Set[Cognate]();
    for( (w,g) <- changes.zipWithIndex if w != -1 && w < current.length) {
      val affScore = affinities(g)(w);
      // i.e. if -log prob of recommended attachment is worse than going it alone, just go it alone
      println(current(w) + " " +  affScore + " " + otherLanguages(g));
      if(affScore >= threshold) {
        score += affinities(g)(w);
      } else {
        changes(g) = -1;
        loners += current(w);
        score += 0.0;
      }
    }

    val augmentedGroups = for {
      (group,g) <- otherLanguages.zipWithIndex;
      newWordIndex = changes(g)
    } yield {
      if(newWordIndex != -1 && newWordIndex < current.length) group + (language -> current(newWordIndex))
      else group;
    }

    val newGroups = augmentedGroups ++ loners.map(c => Map.empty + (language -> c));

    val newKnownLanguages = s.knownLanguages + language;

    val newS = s.copy(groups = newGroups,
                      likelihood = score,
                      knownLanguages = newKnownLanguages);
    newS;
  }

  def iterations = {
    val state = initialState(cognates.filter(_.language == languages(0)),initialFactors);
    val lang= Iterator.continually { languages.iterator.drop(1) ++ Iterator.single(languages(0)) } flatten;
    scanLeft(lang,state)(step(_,_));
  }

  def scanLeft[A,B](it: Iterator[A], b: B)(f: (B,A)=>B):Iterator[B] = new Iterator[B] {
    def hasNext = it.hasNext;
    var c = b;
    def next = {
      val a = it.next;
      c = f(c,a);
      c;
    }
  }

}

/*
class BaselineAdaptive(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language], threshold: Double)
                        extends BaselineX(tree,cognates,languages,threshold) {
  override type MFactors = BigramFactors;
  override def initialFactors: MFactors = new BigramFactors;
}
*/


class BaselineBipartite(tree: Tree,
                        cognates: Seq[Cognate],
                        languages: Seq[Language],
                        treePenalty: Double,
                        initDeathProb: Double,
                        allowSplitting: Boolean)
                        extends Bipartite(tree,cognates,languages,treePenalty,
                                          initDeathProb,allowSplitting,new ConstantBroker(new BigramFactors)) {
}

object Accuracy {

  def precisionAndRecall(languages:Seq[Language], indices: Seq[Map[Language,Int]], numPositive: Counters.IntCounter[(Language,Language)])  = {
    val numRight = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    val numGuesses = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    for {
      map <- indices;
      (l1,i1) <- languages.zipWithIndex;
      (l2,i2) <- languages.zipWithIndex if( i1 < i2);
      truth1 <- map.get(l1);
      truth2 <- map.get(l2)
    } {
      if(truth1 == truth2) {
        numRight(l1->l2) += 1;
        numRight("*"->"*") += 1;
      }
      numGuesses(l1->l2) += 1;
      numGuesses("*"->"*") += 1;
    }

    val precision = for( (pair,total) <- numGuesses) yield (pair,numRight(pair) / total.toDouble);
    val recall = for( (pair,total) <- numPositive) yield (pair,numRight(pair) / total.toDouble);
    (precision.toMap,recall.toMap);
  }

  def indexGold(cogs: Seq[Seq[Cognate]]) = {
    val map = (for( (group,i) <- cogs.zipWithIndex; c <- group) yield (c,i)) toMap;
    map;
  }

  def numberOfPositives(languages: Seq[Language], groups: Seq[Seq[Cognate]]) = {
    val numPositive = scalala.tensor.counters.Counters.IntCounter[(Language,Language)];
    for {
      cogs <- groups;
      set = cogs.map(_.language).toSet;
      (l1,i1) <- languages.zipWithIndex if set contains l1
      (l2,i2) <- languages.zipWithIndex if( i1 < i2) && set.contains(l2)
    } {
      numPositive(l1 -> l2) += 1;
      numPositive("*" -> "*") += 1;
    }

    numPositive;
  }


}

case class Params(treePenalty: Double, initDeathProb: Double, allowDeath: Boolean);


object BipartiteRunner {
  import Accuracy._;

  def main(args: Array[String]) {
    val props = new Properties();
    props.load(new FileInputStream(new File(args(0))));
    val config = Configuration.fromProperties(props);
    val languages = config.readIn[String]("dataset.languages").split(",");

    val dataset_name = config.readIn[String]("dataset.name");
    val dataset = new Dataset(dataset_name,languages);
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

    val rootCompression = readAutomataCompressor(config, "transducers.root");

    val shouldLearn = config.readIn[Boolean]("transducers.learning");
    val initialFactors = new TransducerFactors(tree,alphabet,messageCompression);
    val broker = (
      if(!shouldLearn) new ConstantBroker(initialFactors)
      else new TransducerBroker(tree, alphabet, initialFactors, messageCompression, transducerCompression, rootCompression)
    );


    val bipartite = new Bipartite[TransducerFactors](tree, randomized, languages, treePenalty, initDeathProb, allowDeath, broker);
    val iter = bipartite.iterations;
    val numPositives = numberOfPositives(languages, gold);

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

