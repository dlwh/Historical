package dlwh.cognates

import scalanlp.fst.Alphabet;
import scalanlp.util.Log._;
import Types._;

trait TransducerLearning {
  val transducerCompressor: Compressor[_,(Char,Char)];
  def alphabet: Set[Char];
  def eps = implicitly[Alphabet[Char]].epsilon;
  def tree: Tree;
  type Statistics = Map[(Language,Language),transducerCompressor.Statistics];
  private val edgesToLearn = tree.edges.toSeq;

  def mkFactors(statistics: Statistics):TransducerFactors = {
    globalLog.log(INFO)("Trans in " + memoryString);
    val transducers = Map.empty ++ statistics.mapValues ( ctr =>  transducerCompressor.compress(0.0,ctr));

    val factors = new TransducerFactors(tree,alphabet,transducers) with UniPruning with SafePruning;
    globalLog.log(INFO)("Trans out " + memoryString);
    factors
  }

  def gatherStatistics(ios: Iterator[InsideOutside[TransducerFactors]]) = {
    val trigramStats = for{
      io <- ios
      pair@ (fromL,toL) <- edgesToLearn.iterator;
      trans <- io.edgeMarginal(fromL, toL).iterator
    } yield {
      val allPairs = for {
        a <- alphabet + eps;
        b <- alphabet + eps;
        if a != eps || b != eps
      } yield (a,b);

      val cost = transducerCompressor.gatherStatistics(allPairs,trans.fst);

      (fromL,toL) -> cost._1;
    }

    import collection.mutable.{Map=>MMap}
    val stats = MMap[(Language,Language),transducerCompressor.Statistics]();
    for ( (lpair,ctr) <- trigramStats)  {
      stats(lpair) = stats.get(lpair).map(transducerCompressor.interpolate(_,1,ctr,1)).getOrElse(ctr);
    }

    Map.empty ++ stats;
  }

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      langs -> transducerCompressor.interpolate(as,1-eta,bs,eta);
    }

    newStats;
  }
}
