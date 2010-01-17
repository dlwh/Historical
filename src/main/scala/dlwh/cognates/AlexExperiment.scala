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

  val initialMatchCounts = 100.0;
  val initialSubCounts = 5.0
  val initialInsCounts = 2.0;

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
    val initialStatistics = mkInitialStatistics;

    val batches = mkBatches;

    val finalStatistics = batches.foldLeft(initialStatistics) { (statistics,batchgroup) =>
      val factors = mkFactors(statistics);
      val (batch,iter) = batchgroup;
      val ios = mkInsideOutsides(factors,batch);
      val newStatistics = gatherStatistics(ios);
      interpolate(statistics,newStatistics,eta(iter));
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
        println("Marg for " + l);
        System.out.flush();
        val marg = io.marginalFor(l).fsa
        println("Fleshing out " + l);
        System.out.flush();
        val best = oneBest(marg).str.mkString
        l + " " + best + " " + marg.cost
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
    val ring = new OneBestSemiring[Char,Double];
    import ring._;
    a.reweight(promote _, promoteOnlyWeight _ ).cost
  }

  def gatherStatistics(ios: Iterator[InsideOutside[TransducerFactors]]) = {
    val trigramStats = for{
      io <- ios
      (fromL,toL) <- edgesToLearn.iterator
    } yield {
      val ring = new UnigramSemiring[(Char,Char)]( allPairs, ('#','#'), cheatOnEquals= true );
      import ring._;
      val trans = io.edgeMarginal(fromL, toL);
      val cost = trans.fst.reweight(promote _, promoteOnlyWeight _ ).cost;

      (fromL,toL) -> cost.decode
    }

    import collection.mutable.{Map=>MMap}
    type APair = (Char,Char)
    val stats = MMap[(Language,Language),LogDoubleCounter[(Char,Char)]]();
    for ( (lpair,ctr) <- trigramStats)  {
      val inner = stats.getOrElseUpdate(lpair,LogDoubleCounter());
      for( ( t, v) <- ctr) {
        inner(t) = logSum(inner(t),v);
      }
    }

    Map.empty ++ stats.mapValues( (a: LogDoubleCounter[(Char,Char)]) => logNormalize(a) );
  }

  private type Statistics = Map[(Language,Language),LogDoubleCounter[(Char,Char)]];

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

  def mkInitialStatistics = {
    val counts = LogDoubleCounter[(Char,Char)]();
    for {
      (a,b) <- allPairs
      count = if(a == b) initialMatchCounts else if(a == eps || b == eps) initialInsCounts else initialSubCounts
      logCost = Math.log(count)
    } {
      counts(a->b) = logCost;
    }

    val ctr = logNormalize(counts);

    val trigramStats = for{
      (fromL,toL) <- edgesToLearn.iterator
    } yield (fromL,toL) -> ctr;

    Map.empty ++ trigramStats;
  }

  def mkFactors(statistics: Map[(Language,Language),LogDoubleCounter[(Char,Char)]]) = {
    val tc = new UniCompression[(Char,Char)](allPairs, ('#','#') ) with NormalizedByFirstChar[Unit,Char];
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