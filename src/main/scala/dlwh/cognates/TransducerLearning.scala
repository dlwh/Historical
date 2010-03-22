package dlwh.cognates

import scalanlp.fst.Alphabet;
import scalanlp.fst.Automaton;
import scalanlp.fst.Transducer
import scalanlp.util.Log._;
import Types._;
import scalanlp.math.Semiring.LogSpace._;
import scalanlp.counters.LogCounters._;

trait TransducerLearning {
  val transducerCompressor: Compressor[_,(Char,Char)];
  val rootCompressor: Compressor[_,Char];
  def alphabet: Set[Char];
  def eps = implicitly[Alphabet[Char]].epsilon;
  def tree: Tree;
  type Statistics = Map[(Language,Language),transducerCompressor.Statistics];
  type RootStats = rootCompressor.Statistics;
  private val edgesToLearn = tree.edges.toSeq;
  private lazy val allPairs = for {
    a <- alphabet + eps;
    b <- alphabet + eps;
    if a != eps || b != eps
  } yield (a,b);


  def initialMatchCounts = -68;
  def initialSubCounts = initialMatchCounts-4;
  def initialDelCounts = initialMatchCounts-6;

  def mkRoot(stats: RootStats): Automaton[Double,_,Char] = {
    rootCompressor.compress(0.0,stats);
  }

  def mkTransducers(statistics: Statistics):Map[(Language,Language),Transducer[Double,_,Char,Char]] = {
    globalLog.log(INFO)("Trans in " + memoryString);
    val transducers:Map[(Language,Language),Transducer[Double,_,Char,Char]] = {
      Map.empty ++ statistics.mapValues ( ctr =>  transducerCompressor.compress(0.0,ctr):Transducer[Double,_,Char,Char]);
    }
    transducers
  }

  def gatherStatistics(ios: Iterator[InsideOutside[TransducerFactors]]): (Statistics,RootStats) = {

    val trigramStats = (TaskExecutor.doTasks {for{
      io <- ios.toSeq
    } yield { () =>
      println(io.assignments);
      val edges = (for { pair@ (fromL,toL) <- edgesToLearn.iterator;
        trans <- io.edgeMarginal(fromL, toL).iterator
      } yield {

        val cost = transducerCompressor.gatherStatistics(allPairs,trans.fst);
        assert(!cost._2.isInfinite);

        (fromL,toL) -> cost._1;
      } ).toSeq;

      val root = io.rootMarginal;
      (edges,rootCompressor.gatherStatistics(alphabet + eps,root.fsa)._1);
   } })

    import collection.mutable.{Map=>MMap}
    val stats = MMap[(Language,Language),transducerCompressor.Statistics]();
    var rootStats: Option[RootStats] = None;
    for ( (ts,rStats) <- trigramStats) {
      if(rootStats.isEmpty) {
        rootStats = Some(rStats)
      } else {
        rootStats = Some(rootCompressor.interpolate(rootStats.get,1,rStats,1));
      }
      for( (lpair,ctr) <- ts) {
        if(stats.contains(lpair)) stats(lpair) = transducerCompressor.interpolate(stats(lpair),1,ctr,1);
        else stats(lpair) = ctr;
      }
    }

    val smoothingCounter = LogDoubleCounter[(Char,Char)]();
    for( p@(a,b) <- allPairs) {
      smoothingCounter(p) = if(a == b) initialMatchCounts else if(a == eps || b == eps) initialDelCounts else initialSubCounts;
    }

    val charSmoothing = LogDoubleCounter[Char]();
    for( a <- alphabet) {
      charSmoothing(a) = initialMatchCounts;
    }


    val finalTransducerStats = Map.empty ++ stats.mapValues(transducerCompressor.smooth(_,smoothingCounter));
    val finalRootStats = rootCompressor.smooth(rootStats.get,charSmoothing);
    (finalTransducerStats,finalRootStats);
  }

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      langs -> transducerCompressor.interpolate(as,1-eta,bs,eta);
    }

    newStats;
  }
}
