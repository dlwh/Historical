package dlwh.cognates

import scalanlp.fst._
import scalanlp.stats.sampling.Rand;
import scala.collection.mutable.ArrayBuffer
import scalala.Scalala._;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import Types._;
import scalanlp.util.Index
import scalanlp.util.Log._;
import scalanlp.optimize.KuhnMunkres;

abstract class Bipartite(val tree: Tree, cognates: Seq[Cognate], languages: Seq[Language]) {
  type MFactors <: Factors
  case class State(permutations: Map[Language,Seq[Cognate]], factors: MFactors, likelihood:Double = Double.NegativeInfinity);

  def makeIO(s:State, otherLanguages: Map[Language,Seq[Cognate]], j: Int) = {
    val words = Map.empty ++ otherLanguages.iterator.map { case (l,seq) => (l,seq(j).word) }
    println("Looking at " + words);
    val io = new InsideOutside(tree, s.factors, words);
    io;
  }

  def initialFactors: MFactors

  def initialState(words :Seq[Cognate], factors: MFactors): State = {
    val map = Map.empty ++ words.groupBy(_.language)
    State(map, factors);
  }

  final def step(s: State, language: Language):State = {
    val current = s.permutations.get(language) getOrElse cognates.filter(_.language == language);
    import s._;
    val otherLanguages = permutations - language;
    println(language);
    val tasks = for ( j <- 0 until current.length) yield { () =>
      val marg = makeIO(s,otherLanguages,j).marginalFor(language).get;
      val aff = new Array[Double](current.length);
      for ( i <- 0 until current.length ) {
        aff(i) = marg(current(i).word);
        assert(!aff(i).isNaN);
      }
      (j,aff);
    }

    val affinities = new Array[Array[Double]](current.length);
    TaskExecutor.doTasks(tasks) foreach { case (j,aff) =>
      affinities(j) = aff;
    }

    val (changes,score) = KuhnMunkres.extractMatching(affinities.map(x => x:Seq[Double]).toSeq);
    // repermute our current permutation
    val newPermute = changes map current toSeq;
    val newS = s.copy(permutations = otherLanguages + (language -> newPermute), likelihood = score);
    nextAction(newS)
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

class BaselineBipartite(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language]) extends Bipartite(tree,cognates,languages) {
  override type MFactors = BigramFactors;
  override def initialFactors: MFactors = new BigramFactors;
}

class NoLearningBipartite(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language]) extends Bipartite(tree,cognates,languages) {
  val alphabet = Set.empty ++ cognates.iterator.flatMap(_.word.iterator);
  type MFactors = TransducerFactors

  def initialFactors = new TransducerFactors(tree,alphabet) with UniPruning
}



class TransBipartite(tree: Tree, cognates: Seq[Cognate], languages: Seq[Language]) extends Bipartite(tree,cognates,languages) with TransducerLearning {
  val alphabet = Set.empty ++ cognates.iterator.flatMap(_.word.iterator);

  type MFactors = TransducerFactors
  val transducerCompressor = new UniCompression(('#','#')) with NormalizedByFirstChar[Unit,Char];

  override def nextAction(s: State) = learnFactors(s);

  def learnFactors(s: State):State = if(languages.size == s.permutations.size) {
    var ll = 0.0;
    val numGroups = s.permutations.valuesIterator.next.length;
    val ios = for(i <- 0 until numGroups iterator) yield {
      val io = makeIO(s,s.permutations,i);
      ll += io.likelihood;
      io;
    }

    val stats = gatherStatistics(ios);
    val newFactors = mkFactors(stats);
    s.copy(factors = newFactors, likelihood = ll);
  } else s

  def initialFactors = new TransducerFactors(tree,alphabet) with PosUniPruning

  def mkFactors(statistics: Statistics):TransducerFactors = {
    val transducers = mkTransducers(statistics);
    val factors = new TransducerFactors(tree,alphabet,transducers) with PosUniPruning
    globalLog.log(INFO)("Trans out " + memoryString);
    factors
  }

}

object Accuracy {
  def permutationAccuracy(a: Seq[Int], b: Seq[Int]) = {
    var numRight = 0;
    var numWrong = 0;
    for( (aa,bb) <- a zip b) {
      if(aa == bb) numRight += 1;
      else numWrong += 1;
    }
    numRight * 1.0 / a.length;
  }

  def doPermutations(indices: Map[Language,Seq[Int]])  = {
    for( (l1,a) <- indices;
        (l2,b) <- indices.dropWhile(_._1!= l1).drop(1)
       ) yield ( (l1,l2),permutationAccuracy(a,b));
  }

  def indexGold(cogs: Seq[Seq[Cognate]]) = {
    val map = (for( (group,i) <- cogs.zipWithIndex; c <- group) yield (c,i)) toMap;
    map;
  }


}


trait BipartiteRunner {
  import Accuracy._;

  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String]): Bipartite

  def main(args: Array[String]) {
    val languages = args(1).split(",");
    val dataset = new Dataset(args(0),languages);
    val gold = dataset.cognates;
    val goldIndices = indexGold(gold);
    val data = gold.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val iter = bip(dataset.tree, randomized, languages).iterations;
    for( state <- iter.take(1000)) {
      val numGroups = state.permutations.valuesIterator.map(_.length).reduceLeft(_ max _);
      for(g <- 0 until numGroups) {
        val cognates = for(cogs <- state.permutations.valuesIterator) yield cogs(g);
        println(cognates.mkString(","));
      }
      println("Likelihood" + state.likelihood);
      val accuracies = doPermutations(state.permutations.mapValues(_ map goldIndices).toMap);
      println(accuracies);
    }
  }
}

object RunBipartite extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String]) = new TransBipartite(tree,cogs,languages);
}

object RunBaseline extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String]) = new BaselineBipartite(tree,cogs,languages);

}

object RunNoLearning extends BipartiteRunner {
  def bip(tree: Tree, cogs: Seq[Cognate], languages: Seq[String]) = new NoLearningBipartite(tree,cogs,languages);
}
