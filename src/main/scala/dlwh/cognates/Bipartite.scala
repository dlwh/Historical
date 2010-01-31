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
import scalanlp.optimize.CompetitiveLinking;


class Bipartite(tree: Tree, cognates: Seq[Cognate], languages: Set[String], alpha: Double = 0.7, batchSize: Int = 3, nIter:Int = 100) {
  require(alpha < 1 && alpha >= 0.5);
  require(batchSize >= 1);

  val initialMatchCounts = 80.0;
  val initialSubCounts = 5.0;
  val initialInsCounts = 1.0;

  private val alphabet = Set.empty ++ cognates.iterator.flatMap(_.word.iterator);
  private val eps = implicitly[Alphabet[Char]].epsilon;
  val transCompr = new UniCompression[(Char,Char)]( ('#','#') ) with NormalizedByFirstChar[Unit,Char];
  type Statistics = Map[(Language,Language),transCompr.Statistics];

  private val edgesToLearn = tree.edges.toSeq;

  private val allPairs = for {
    a <- alphabet + eps;
    b <- alphabet + eps;
    if a != eps || b != eps
  } yield (a,b);

  def eta(iter: Int) = Math.pow(iter+2,-alpha);

  def makeIO(s:State, otherLanguages: Map[Language,Seq[Cognate]], j: Int) = {
    val words = Map.empty ++ otherLanguages.iterator.map { case (l,seq) => (l,seq(j).word) }
    println("looking for a new friend for" + words);
    val io = new InsideOutside(tree, s.factors, words);
    assert(!io.likelihood.isNaN);
    assert(!io.likelihood.isInfinite);
    io;
  }

  case class State(permutations: Map[Language,Seq[Cognate]], factors: TransducerFactors, likelihood:Double = Double.NegativeInfinity);

  def step(s: State, language: Language):State = (for( current <- s.permutations.get(language)) yield {
    import s._;
    val otherLanguages = permutations - language;
    val affinities = Array.ofDim[Double](current.length,current.length);
    println(language);
    for ( j <- 0 until current.length) {
      println("GC1 in" + memoryString);
      System.gc();
      println("GC1 out" + memoryString);
      val marg = makeIO(s,otherLanguages,j).marginalFor(language).get;
      println("GC2 in" + memoryString);
      System.gc();
      println("GC2 out" + memoryString);
      for ( i <- 0 until current.length ) {
        println(current(i).word);
        affinities(j)(i) = marg(current(i).word);
        assert(!affinities(j)(i).isNaN);
        println(current(i).word,affinities(j)(i));
      }
    }
    val (changes,score) = CompetitiveLinking.extractMatching(affinities.map(x => x:Seq[Double]).toSeq);
    // repermute our current permutation
    val newPermute = changes map current toSeq;
    s.copy(permutations = otherLanguages + (language -> newPermute), likelihood = score);
  }) getOrElse(s);

  def initialState(words :Seq[Cognate], factors: TransducerFactors): State = {
    val map = Map.empty ++ words.groupBy(_.language)
    State(map, factors);
  }
  def initialFactors = new TransducerFactors(tree,alphabet) with PosUniPruning;

  def iterations = {
    val state = initialState(cognates,initialFactors);
    val lang= Iterator.continually { languages.iterator } flatten;
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

  def oneBest(a: Psi) = {
    import scalanlp.math.Semiring.LogSpace._;
    val obring = new OneBestSemiring[Char,Double];
    import obring.{ring,promote,promoteOnlyWeight};
    a.reweight(promote _, promoteOnlyWeight _ ).cost
  }

  def gatherStatistics(ios: Iterator[InsideOutside[TransducerFactors]]) = {
    val trigramStats = for{
      io <- ios
      pair@ (fromL,toL) <- edgesToLearn.iterator;
      trans <- io.edgeMarginal(fromL, toL).iterator
    } yield {
      val alphabet = io.alphabet;
      val allPairs = for {
        a <- alphabet + eps;
        b <- alphabet + eps;
        if a != eps || b != eps
      } yield (a,b);

      globalLog.log(INFO)("BG in" + memoryString);
      println(pair);
      val cost = transCompr.gatherStatistics(allPairs,trans.fst);
      globalLog.log(INFO)("BG out" + memoryString);
      globalLog.log(INFO)("GC2 in" + memoryString);
      System.gc();
      globalLog.log(INFO)("GC2 out" + memoryString);

      (fromL,toL) -> cost._1;
    }

    import collection.mutable.{Map=>MMap}
    val stats = MMap[(Language,Language),transCompr.Statistics]();
    for ( (lpair,ctr) <- trigramStats)  {
      stats(lpair) = stats.get(lpair).map(transCompr.interpolate(_,1,ctr,1)).getOrElse(ctr);
    }

    Map.empty ++ stats;
  }

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      langs -> transCompr.interpolate(as,1-eta,bs,eta);
    }

    newStats;
  }

  def mkFactors(statistics: Statistics) = {
    globalLog.log(INFO)("Trans in " + memoryString);
    val transducers = Map.empty ++ statistics.mapValues ( ctr =>  transCompr.compress(0.0,ctr));

    val factors = new TransducerFactors(tree,alphabet,transducers) with PosUniPruning;
    globalLog.log(INFO)("Trans out " + memoryString);
    factors
  }
}


object RunBipartite {

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


  def main(args: Array[String]) {
    val languages = args(1).split(",");
    val dataset = new Dataset(args(0),languages);
    val gold = dataset.cognates;
    val goldIndices = indexGold(gold);
    val data = gold.flatten;
    val randomized = Rand.permutation(data.length).draw().map(data);
    val iter = new Bipartite(dataset.tree, randomized, Set.empty ++ languages).iterations;
    for( state <- iter.take(1000)) {
      val numGroups = state.permutations.valuesIterator.map(_.length).reduceLeft(_ max _);
      for(g <- 0 until numGroups) {
        val cognates = for(l <- languages) yield state.permutations(l)(g);
        println(cognates.mkString(","));
      }
      println("Likelihood" + state.likelihood);
      val accuracies = doPermutations(state.permutations.mapValues(_ map goldIndices).toMap);
      println(accuracies);
    }
  }
}
