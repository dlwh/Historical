package dlwh.newcognates

import scalanlp.fst.Alphabet;

import scalanlp.fst.Transducer
import scalanlp.util.Log._;
import scalanlp.math.Semiring.LogSpace._;
import scalala.tensor.counters.LogCounters._;
import scalanlp.concurrent.ParallelOps._
import dlwh.cognates.Compressor

trait TransducerLearning {
  val transducerCompressor: Compressor[_,(Char,Char)];
  //val rootCompressor: Compressor[_,Char];
  def alphabet: Set[Char];
  def eps = implicitly[Alphabet[Char]].epsilon;
  def tree: Tree;
  type Statistics = Map[(Language,Language),transducerCompressor.Statistics];
 // type RootStats = rootCompressor.Statistics;
  private val edgesToLearn = tree.edges.toSeq;
  private lazy val allPairs = for {
    a <- alphabet + eps;
    b <- alphabet + eps;
    if a != eps || b != eps
  } yield (a,b);


  def initialMatchCounts = -6;
  def initialSubCounts = initialMatchCounts-4;
  def initialDelCounts = initialMatchCounts-6;

  /*
  def mkRoot(stats: RootStats): Automaton[Double,_,Char] = {
    rootCompressor.compress(0.0,stats);
  }
  */

  def mkTransducers(statistics: Statistics):Map[(Language,Language),Transducer[Double,_,Char,Char]] = {
    globalLog.log(INFO)("Trans in " + memoryString);
    val transducers:Map[(Language,Language),Transducer[Double,_,Char,Char]] = {
      Map.empty ++ statistics.mapValues ( ctr =>  transducerCompressor.compress(0.0,ctr):Transducer[Double,_,Char,Char]);
    }
    transducers
  }

  def gatherStatistics(ios: Iterator[TreeInference[TransducerFactors]#BeliefState]): (Statistics) = {
    globalLog.log(INFO)("Gather Stats");
    val stats = ios.toIndexedSeq.par.mapReduce( { io =>
      val edges = for { pair@ (fromL,toL) <- edgesToLearn.iterator;
       () = println(fromL,toL);
        edge <- io.edgeMarginal(fromL, toL).iterator
      } yield {
        val cost = transducerCompressor.gatherStatistics(allPairs,edge.trans);
        assert(!cost._2.isInfinite,edge.trans);

        (fromL,toL) -> cost._1;
      }
      edges toMap;
    },
    { (costs1:Map[(Language,Language),transducerCompressor.Statistics],costs2:Map[(Language,Language),transducerCompressor.Statistics]) =>
      import collection.mutable.{Map=>MMap}
      val stats = MMap[(Language,Language),transducerCompressor.Statistics]() ++ costs1;
      for( (lpair,ctr) <- costs2) {
        if(stats.contains(lpair)) stats(lpair) = transducerCompressor.interpolate(stats(lpair),1,ctr,1);
        else stats(lpair) = ctr;
      }
      stats.toMap
    });

    val smoothingCounter = LogDoubleCounter[(Char,Char)]();
    for( p@(a,b) <- allPairs) {
      smoothingCounter(p) = if(a == b) initialMatchCounts else if(a == eps || b == eps) initialDelCounts else initialSubCounts;
    }

    val charSmoothing = LogDoubleCounter[Char]();
    for( a <- alphabet) {
      charSmoothing(a) = initialMatchCounts;
    }


    val finalTransducerStats = Map.empty ++ stats.mapValues(transducerCompressor.smooth(_,smoothingCounter));
    (finalTransducerStats);
  }

  def interpolate(a: Statistics, b: Statistics, eta: Double) = {
    val newStats = for( (langs,as) <- a; bs = b(langs)) yield {
      langs -> transducerCompressor.interpolate(as,1-eta,bs,eta);
    }

    newStats;
  }
}
