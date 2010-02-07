package dlwh.cognates

import scalanlp.fst.Alphabet;
import scalanlp.fst.Transducer
import scalanlp.util.Log._;
import Types._;
import scalanlp.counters.LogCounters._;

trait TransducerLearning {
  val transducerCompressor: Compressor[_,(Char,Char)];
  def alphabet: Set[Char];
  def eps = implicitly[Alphabet[Char]].epsilon;
  def tree: Tree;
  type Statistics = Map[(Language,Language),transducerCompressor.Statistics];
  private val edgesToLearn = tree.edges.toSeq;
  private lazy val allPairs = for {
    a <- alphabet + eps;
    b <- alphabet + eps;
    if a != eps || b != eps
  } yield (a,b);


  def initialMatchCounts = -30;
  def initialSubCounts = initialMatchCounts-4;
  def initialDelCounts = initialMatchCounts-6;

  def mkTransducers(statistics: Statistics):Map[(Language,Language),Transducer[Double,_,Char,Char]] = {
    globalLog.log(INFO)("Trans in " + memoryString);
    val transducers:Map[(Language,Language),Transducer[Double,_,Char,Char]] = {
      Map.empty ++ statistics.mapValues ( ctr =>  transducerCompressor.compress(0.0,ctr):Transducer[Double,_,Char,Char]);
    }
    transducers
  }

  def gatherStatistics(ios: Iterator[InsideOutside[TransducerFactors]]): Statistics = {

    val trigramStats: Iterator[((Language,Language),transducerCompressor.Statistics)] = (TaskExecutor.doTasks {for{
      io <- ios.toSeq
    } yield { () =>
      (for { pair@ (fromL,toL) <- edgesToLearn.iterator;
        trans <- io.edgeMarginal(fromL, toL).iterator
      } yield {

        val cost = transducerCompressor.gatherStatistics(allPairs,trans.fst);
        assert(!cost._2.isInfinite);

        println("Here" + pair + cost._2);
        (fromL,toL) -> cost._1;
      } ).toSeq iterator
   } }).iterator.flatten

    import collection.mutable.{Map=>MMap}
    val stats = MMap[(Language,Language),transducerCompressor.Statistics]();
    for ( (lpair,ctr) <- trigramStats)  {
      println(lpair);
      stats(lpair) = stats.get(lpair).map(transducerCompressor.interpolate(_,1,ctr,1)).getOrElse(ctr);
    }

    val smoothingCounter = LogDoubleCounter[(Char,Char)]();
    for( p@(a,b) <- allPairs) {
      smoothingCounter(p) = if(a == b) initialMatchCounts else if(a == eps || b == eps) initialDelCounts else initialSubCounts;
    }

    Map.empty ++ stats.mapValues(transducerCompressor.smooth(_,smoothingCounter));
  }

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      langs -> transducerCompressor.interpolate(as,1-eta,bs,eta);
    }

    newStats;
  }
}
