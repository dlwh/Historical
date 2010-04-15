package dlwh.cognates;

import scalanlp.fst._;
import scalanlp.util.Index;
import scalala.tensor.counters.LogCounters.{logSum => _, _};
import scalala.Scalala._;
import scala.collection.mutable.PriorityQueue;
import scala.reflect.OptManifest;
import scalanlp.math.Semiring.LogSpace._;
import Automaton._;


abstract class PosUniCompression[T](maxLength: Int, val beginningUnigram: T)(implicit alpha: Alphabet[T], implicit man:
                                      OptManifest[T]) extends Compressor[Int,T] with ArcCreator[Int,T] {
  require(maxLength >= 1);

  def alphabet = implicitly[Alphabet[T]];

  type Statistics = Seq[LogDoubleCounter[T]];

  def gatherStatistics(validChars: Set[T], auto: Automaton[Double,_,T]):(Statistics,Double) = {
    val tgs = new PositionalUnigramSemiring(maxLength, validChars, beginningUnigram, cheatOnEquals=true);
    import tgs._;
    import ring._;
    val cost = auto.reweight(promote[Any] _ , promoteOnlyWeight _).cost;
    (cost.decode,cost.totalProb);
  }

  def interpolate(stats1: Statistics, eta1: Double, stats2: Statistics, eta2 :Double):Statistics = {
    val logEta = Math.log(eta2);
    for ( (a,b) <- stats1 zip stats2) yield {
      val c = (a + Math.log(eta1)) value;
      for( (k,v) <- b) {
        c(k) = logSum(c(k),v + logEta);
      }
      c
    }
  }

  def transformCounts(stats: Statistics)(f: (T,Double)=>Double):Statistics = {
    val r = Seq.fill(stats.length)(LogDoubleCounter[T]);
    for((ctr,i) <- stats zipWithIndex;
        (k,v) <- ctr) {
      r(i)(k) = f(k,v);
    }
    r;
  }

  
  def smooth(stats: Statistics, counts: LogDoubleCounter[T]): Statistics = {
    val res = stats.map(_.copy);
    for( ctr <- res; (k,v) <- counts) {
      ctr(k) = logSum(ctr(k),v);
    }
    res;
  }

  def compress(chars: Set[T], auto: Automaton[Double,_,T]):Automaton[Double,Int, T] = {
    val (stats,cost) = gatherStatistics(chars,auto);
    compress(cost,stats);
  }

  def destinationFor(i: Int, t: T) = i + 1;

  def compress(prob: Double, counts: Seq[LogDoubleCounter[T]]): Automaton[Double,Int,T] = {

    // 1 is always just #
    val arcs = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      if ctr.logTotal > Double.NegativeInfinity;
      arc <- arcsForCounter(i,ctr)
      _ = assert(!arc.weight.isNaN)
    } yield arc;


    val endingWeights = for {
      (ctr,i) <- counts.iterator.drop(1).zipWithIndex;
      if ctr.logTotal > Double.NegativeInfinity;
      w = finalWeight(i,ctr);
      _ = assert(!w.isNaN)
      if w != Double.NegativeInfinity
    } yield (i,w);

    val startState = 0;

    val auto = Automaton.intAutomaton(Map(startState->prob),Map.empty ++ endingWeights)(
      (arcs).toSeq :_*
    );
    auto
  }
}