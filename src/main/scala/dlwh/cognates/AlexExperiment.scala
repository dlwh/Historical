package dlwh.cognates

import scalanlp.fst._
import scalanlp.stats.sampling.Rand;
import scalala.Scalala._;
import scalanlp.counters.LogCounters._;
import scalanlp.math.Numerics._;
import Types._;

class AlexExperiment(tree: Tree, cognates: Seq[Seq[Cognate]], alpha: Double = 0.7, batchSize: Int = 3, nIter:Int = 100) {
  require(alpha < 1 && alpha >= 0.5);
  require(batchSize >= 1);

  val initialMatchCounts = 80.0;
  val initialSubCounts = 5.0;
  val initialInsCounts = 1.0;

  private val alphabet = Set.empty ++ cognates.iterator.flatMap(_.iterator).flatMap(_.word.iterator);
  private val eps = implicitly[Alphabet[Char]].epsilon;

  private val edgesToLearn = tree.edges.toSeq;

  private val allPairs = for {
    a <- alphabet + eps;
    b <- alphabet + eps;
    if a != eps || b != eps
  } yield (a,b);

  def eta(iter: Int) = Math.pow(iter+2,-alpha);

  def run = {
    val initialStatistics = initialIteration;

    val batches = mkBatches;

    val finalStatistics = batches.foldLeft(initialStatistics) { (statistics,batchgroup) =>
      val factors = mkFactors(statistics);
      val (batch,iter) = batchgroup;
      val ios = mkInsideOutsides(factors,batch);
      val newStatistics = gatherStatistics(ios);
      val inter = interpolate(statistics,newStatistics,eta(iter));
      for( (languagePair,ctr1) <- inter.iterator) {
        println(languagePair + " => {");
        for( (context,ctr2) <- ctr1.rows) {
          println(context + "->");
          println(logNormalize(ctr2));
        }
        println("}");
      };
      inter
    }

    finalStatistics
  }


  def mkBatches = {
    for {
      iter <- 1 to nIter iterator;
      permutation = (Rand.permutation(cognates.size).draw).view map cognates;
      (group,pos) <- permutation.iterator.sliding(batchSize,batchSize) zipWithIndex
    } yield (group,iter * cognates.size + pos);
  }

  def mkInsideOutsides(factors: TransducerFactors, cognates: Seq[Seq[Cognate]]) = {
    val fixed = new Fixed(tree, factors);
    val ios = for( cogs <- cognates.iterator) yield {
      val preTree = tree map { l =>
        val oneBest = cogs.find(_.language == l).map(_.word) getOrElse "<>";
        l + " " + oneBest;
      }
      println(preTree);
      val io = fixed.inferenceOn(cogs)

      val labeledTree = tree map { l =>
        System.out.println(l);
        val best = for( marg <- io.reconstruction(l);
                        best = oneBest(marg.fsa).str.mkString
                      ) yield (best,marg.fsa.cost);
        l + " " + best;
      }
      println(cogs);
      System.out.flush();
      println(labeledTree);
      io
    }

    ios;
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
      val uRing = new BigramSemiring[(Char,Char)]( allPairs, ('#','#'), cheatOnEquals= true );
      import uRing._;
      println(pair);
      val cost = trans.fst.reweight(promote _, promoteOnlyWeight _ ).cost;

      (fromL,toL) -> cost.counts
    }

    import collection.mutable.{Map=>MMap}
    type APair = (Char,Char)
    val stats = MMap[(Language,Language),LogPairedDoubleCounter[(Char,Char),(Char,Char)]]();
    for ( (lpair,ctr) <- trigramStats)  {
      val inner = stats.getOrElseUpdate(lpair,LogPairedDoubleCounter());
      for( ( t, v) <- ctr) {
        inner(t) = logSum(inner(t),v);
      }
    }

    Map.empty ++ stats.mapValues( (a: LogPairedDoubleCounter[(Char,Char),(Char,Char)]) => logNormalizeRows(a) );
  }

  private type Statistics = Map[(Language,Language),LogPairedDoubleCounter[(Char,Char),(Char,Char)]];

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val logEta = Math.log(eta);
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      val c = (as + Math.log(1 - eta)) value;
      for( (k,v) <- bs) {
        c(k) = logSum(c(k),v + logEta);
      }
      langs -> c
    }

    newStats;
  }

  def initialIteration = {
    val factors = new TransducerFactors(tree,alphabet) with PosUniPruning;
    val ios = mkInsideOutsides(factors,cognates);
    val initialStatistics =  gatherStatistics(ios);
    initialStatistics
  }

  def mkFactors(statistics: Statistics) = {
    val tc = new BiCompression[(Char,Char)](0.0,20,allPairs, ('#','#') ) with NormalizedByFirstChar[Option[(Char,Char)],Char];
    val transducers = Map.empty ++ statistics.mapValues ( ctr =>  tc.compress(0.0,ctr));

    new TransducerFactors(tree,alphabet,transducers) with PosUniPruning;
  }
}


object RunAlexExperiment {
  def main(args: Array[String]) {
    val dataset = new Dataset(args(0),args(1).split(","));
    new AlexExperiment(dataset.tree, dataset.cognates).run;
  }
}
