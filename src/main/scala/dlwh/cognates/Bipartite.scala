package dlwh.cognates

import scalanlp.fst._
import scalanlp.stats.sampling.Rand;
import scala.actors.Future
import scala.collection.mutable.ArrayBuffer
import scalala.Scalala.{iArrayToPartialMap=>_,iArrayToVector=>_,_};
import scalanlp.counters.LogCounters._;
import scalanlp.counters.Counters;
import scalanlp.math.Numerics._;
import Types._;
import scalanlp.util.Index
import scalanlp.util.Log._;
import fig.basic.BipartiteMatcher;

abstract class Bipartite(val tree: Tree, cognates: Seq[Cognate], languages: Seq[Language], treePenalty: Double, initDeathProb: Double, allowSplitting: Boolean) {
  type MFactors <: Factors
  case class State(groups: Seq[Map[Language,Cognate]],
                   factors: MFactors,
                   deathScores:Map[(Language,Language),Double] = Map().withDefaultValue(initDeathProb),
                   likelihood:Double = Double.NegativeInfinity,
                   knownLanguages: Set[Language] = Set.empty);

  def makeIO(s:State, words: Map[Language,Cognate]) = {
    println("Looking at " + words);
    val io = new InsideOutside(tree, s.factors, Map.empty ++ words.mapValues(_.word));
    io;
  }

  def treeScore(s: State, io: InsideOutside[MFactors]):Double = (
    io.treePrior(s.deathScores)
  )

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
    val prior = treeScore(s,io.include(language,"<dummy>"));
    assert(!prior.isNaN)

    val aff = new Array[Double](words.length);
    for ( i <- 0 until words.length ) {
      aff(i) = (marg(words(i).word) + prior);
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

  def baselineScores(s: State, language: Language, words: Seq[Cognate]) = {
    val arr = calculateAffinities(s,Map.empty,language,words);
    for( i <- 0 until arr.length) {
      arr(i) -= treePenalty;
    }
    arr
  }

  final def step(s: State, language: Language):State = {
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

    val deathProbs = learnDeathProbs(s,newGroups);
    println(deathProbs);

    val newS = s.copy(groups = newGroups,
                      likelihood = score,
                      deathScores = deathProbs,
                      knownLanguages = s.knownLanguages + language);
    nextAction(newS)
  }

  def learnDeathProbs(s: State, groups: Seq[Map[Language,Cognate]]) = {
    val deathCounts = scalanlp.counters.Counters.IntCounter[(Language,Language)];
    val availableCounts = scalanlp.counters.Counters.IntCounter[(Language,Language)];
    for {
      group <- groups;
      io = makeIO(s,group);
      (lPair,lastOnPath) <- io.pathsToFringe
    } {
      availableCounts(lPair) += 1;
      if(lastOnPath) deathCounts(lPair) += 1;
    }

    println(deathCounts,availableCounts);
    val normalized = for {
      (pair,total) <- availableCounts
      deaths = deathCounts(pair)
    } yield (pair,(deaths + initDeathProb) / (total + 1.0));

    normalized.toMap.withDefaultValue(initDeathProb);
  }

  protected def nextAction(state: State): State = state;

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

class BaselineBipartite(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language], treePenalty: Double, initDeathProb: Double, allowSplitting: Boolean)
                        extends Bipartite(tree,cognates,languages,treePenalty,initDeathProb,allowSplitting) {
  override type MFactors = BigramFactors;
  override def initialFactors: MFactors = new BigramFactors;
}

class NoLearningBipartite(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language], treePenalty: Double, initDeathProb: Double, allowSplitting: Boolean)
                      extends Bipartite(tree,cognates,languages,treePenalty,initDeathProb,allowSplitting) {
  val alphabet = Set.empty ++ cognates.iterator.flatMap(_.word.iterator);
  type MFactors = TransducerFactors

  def initialFactors:TransducerFactors = new TransducerFactors(tree,alphabet) with PosUniPruning
}

class TransBipartite(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language], treePenalty: Double, initDeathProb: Double,allowSplitting:Boolean)
                     extends Bipartite(tree,cognates,languages,treePenalty,initDeathProb,allowSplitting) with TransducerLearning {
  val alphabet = Set.empty ++ cognates.iterator.flatMap(_.word.iterator);

  type MFactors = TransducerFactors
  val transducerCompressor = new UniCompression(('#','#')) with NormalizedByFirstChar[Unit,Char];

  override def nextAction(s: State) = learnFactors(s);

  def learnFactors(s: State):State = {
    var ll = 0.0;
    val numGroups = s.groups.length;
    val ios = for(i <- 0 until numGroups iterator) yield {
      val io = makeIO(s,s.groups(i));
      io;
    }

    val stats = gatherStatistics(ios);
    val newFactors = mkFactors(stats);
    s.copy(factors = newFactors);
  }

  def initialFactors:TransducerFactors = new TransducerFactors(tree,alphabet) with PosUniPruning;

  def mkFactors(statistics: Statistics):TransducerFactors = {
    val transducers = mkTransducers(statistics);
    val factors = new TransducerFactors(tree,alphabet,transducers) with PosUniPruning;
    globalLog.log(INFO)("Trans out " + memoryString);
    factors
  }

}

object Accuracy {

  def precisionAndRecall(languages:Seq[Language], indices: Seq[Map[Language,Int]], numPositive: Counters.IntCounter[(Language,Language)])  = {
    val numRight = scalanlp.counters.Counters.IntCounter[(Language,Language)];
    val numGuesses = scalanlp.counters.Counters.IntCounter[(Language,Language)];
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
    val numPositive = scalanlp.counters.Counters.IntCounter[(Language,Language)];
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


trait BipartiteRunner {
  import Accuracy._;

  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double): Bipartite

  def main(args: Array[String]) {
    val languages = args(1).split(",");
    val dataset = new Dataset(args(0),languages);
    val gold = dataset.cognates;
    val goldIndices = indexGold(gold);
    val data = gold.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val expectedNumTrees = data.length.toDouble / languages.size;
    //val treePenalty = Math.log( (expectedNumTrees -1) / expectedNumTrees)
    val treePenalty = 5; // Positive actually means a bonus.
    val iter = bip(dataset.tree, randomized, languages,treePenalty,0.5).iterations;
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
}

object RunBipartite extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new TransBipartite(tree,cogs,languages, treePenalty, initDP,false);
  }
}

object RunUniBipartite extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new TransBipartite(tree,cogs,languages, treePenalty, initDP,false) {

      override def initialFactors:TransducerFactors = new TransducerFactors(tree,alphabet) with UniPruning;

      override def mkFactors(statistics: Statistics):TransducerFactors = {
        val transducers = mkTransducers(statistics);
        val factors = new TransducerFactors(tree,alphabet,transducers) with UniPruning;
        globalLog.log(INFO)("Trans out " + memoryString);
        factors
      }
    }
  }
}

object RunAdaptiveBipartite extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new TransBipartite(tree,cogs,languages, treePenalty, initDP,true);
  }
}


object RunBaseline extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new BaselineBipartite(tree,cogs,languages, treePenalty, initDP,false);
  }

}
object RunAdaptiveBaseline extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new BaselineBipartite(tree,cogs,languages, treePenalty, initDP,true);
  }
}



object RunNoLearning extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new NoLearningBipartite(tree,cogs,languages, treePenalty, initDP,false);
  }
}

object RunAdaptiveNoLearning extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new NoLearningBipartite(tree,cogs,languages, treePenalty, initDP,true);
  }
}

object RunUniNoLearning extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String], treePenalty:Double, initDP: Double) = {
    new NoLearningBipartite(tree,cogs,languages, treePenalty, initDP,false) {

      override def initialFactors:TransducerFactors = new TransducerFactors(tree,alphabet) with UniPruning;

    }
  }
}
